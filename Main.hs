--TODO: Vectors and backquotes
--TODO: Left-factor the List, Dotted-list grammar to remove 'try'
-- Type-testing
-- symbol-handling

module Main where
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

instance Show LispVal where
  show = showVal

data LispVal = Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               | Number Integer
               | String String
               | Bool Bool
               | Character Char
               | Float Float

data LispError = NumArgs Integer [LispVal]
                  | TypeMismatch String LispVal
                  | Parser ParseError
                  | BadSpecialForm String LispVal
                  | NotFunction String String
                  | UnboundVar String String
                  | Default String
                  deriving (Show)

instance Error LispError where
  noMsg = Default "Error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"


trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ "args: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type, expected: " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("string?", unaryOp isString),
              ("number?", unaryOp isNumber),
              ("symbol?", unaryOp isSymbol),
              ("equal?",  binaryOp isEqual)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop p singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

-- TODO: Add error conditions
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [a] = return $ f a

binaryOp :: (LispVal -> LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
binaryOp op params = return $ foldl1 op params

isString :: LispVal -> LispVal
isString (String a) = Bool True
isString _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False

isSymbol :: LispVal -> LispVal
isSymbol (Atom  _) = Bool True
isSymbol _ = Bool False

isEqual :: LispVal -> LispVal -> LispVal
isEqual (Number a) (Number b) = Bool $ (a==b)
isEqual _ _ = Bool False

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                          if null parsed
                            then throwError $ TypeMismatch "number" (String n)
                            else return $ fst $ parsed !! 0

unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

parseCharacter :: Parser LispVal
parseCharacter = do
                char '#'
                char '\\'
                y <- letter <|> digit
                return $ Character y

parseString :: Parser LispVal
parseString = do
            char '"'
            y <- many $ special <|> noneOf "\""
            char '"'
            return $ String y

special :: Parser Char
special = char '\\' >> oneOf ['"','a','n','t','\\']


parseAtom :: Parser LispVal
parseAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $ case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            otherwise -> Atom atom

parseNumber' :: Parser LispVal
parseNumber' = fmap (Number . read) $ many1 digit

-- parsePrefix :: Parser LispVal
 -- parsePrefix = dchar '#'

parsePrefix :: Parser Char
parsePrefix = char '#' >> oneOf ['d', 'o', 'h']

parseNumber = parseWithPrefix <|> parseNumber'

parseWithPrefix :: Parser LispVal
parseWithPrefix = do
            prefix <- parsePrefix
            x <- many1 digit
            return $ case prefix of
                'd' -> (Number . read) x
                'h' -> (Number . read) x -- TODO: ReadOct etc

symbol :: Parser Char
symbol = oneOf "#$%&|*+-/:<=>?@^_~"

parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> parseNumber'
          <|> parseBackQuote
          <|> parseUnquote
          <|> parseQuoted
          <|> do
            char '('
            x <- (try parseList) <|> parseDottedList
            char ')'
            return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser LispVal
parseList = fmap List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseBackQuote :: Parser LispVal
parseBackQuote = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]



main :: IO ()
main = do 
	let args = "(+ 2 2)"
	evaled <- return $ liftM show $ readExpr args >>=
		eval
	putStrLn $ extractValue $ trapError evaled

