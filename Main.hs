  {-# OPTIONS -fglasgow-exts #-}
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

data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

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

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
        do unpacked1 <- unpacker arg1
           unpacked2 <- unpacker arg2
           return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- fmap or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

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
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             Bool True  -> eval conseq
             otherwise -> throwError $ TypeMismatch "Bool" result

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
              ("equal?",  binaryOp isEqual),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq", eqv),
              ("eqv", eqv),
              ("equal?", equal)]

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                (and $ map eqvPair $ zip arg1 arg2)
        where eqvPair (a,b) = case eqv [a,b] of 
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- TODO: Why is [] a thing?
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList (_:xs) x] = return $ DottedList (xs) x
cdr [DottedList [xs] x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList


numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> 
        [LispVal] -> ThrowsError LispVal

boolBinop unpacker op args = if length args /= 2 
                            then throwError $ NumArgs 2 args
                            else do left <- unpacker $ (args !! 0)
                                    right <- unpacker $ (args !! 1)
                                    return $ Bool $ left `op` right

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

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
    let args = "(equal? 2 2)"
    evaled <- return $ fmap show $ readExpr args >>=
        eval
    putStrLn $ extractValue $ trapError evaled

