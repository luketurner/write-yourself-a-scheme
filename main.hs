module Main where

import System.Environment
import Numeric
import Monad
import Ratio
import Complex
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

data Couble = Couble (Complex Double) deriving (Show,Eq)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- (a b . c)
             | Number Integer
             | String String
             | Bool Bool
             | Float Float
             | Char Char
             | Ratio Rational
             | Complex Couble
             
instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Char c) = "#\\" ++ [c]
showVal (List l) = "(" ++ (unwords . map showVal) l ++ ")"
showVal (DottedList list tail) = "(" ++ (unwords . map showVal) list ++ " . " ++ showVal tail ++ ")"
showVal c = show c
             
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               
instance Show LispError where show = showError
instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError
               
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args; found values "
                                  ++ (unwords . map showVal) found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- 3.3.5 (add Char parsing)
parseChar :: Parser LispVal
parseChar = do
    char '#'
    char '\\'
    c <- try (string "space" >> return ' ')
        <|> try (string "newline" >> return '\n')
        <|> alphaNum
    return $ Char c

parseString :: Parser LispVal
parseString = do char '"'
                 str <- many chr 
                 char '"'
                 return $ String str
              -- Ex. 3.3.2
              -- Ex. 3.3.3
              where chr = try (char '\\' >> oneOf "\\\"") <|> noneOf "\""

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                    "#t" -> Bool True
                    "#f" -> Bool False
                    otherwise -> Atom atom

parseNumericPrefix :: Parser Char
parseNumericPrefix = option 'd' (char '#' >> oneOf "bohd")

-- Ex. 3.3.4 (handle dec/oct/hex)
parseNumber :: Parser LispVal
parseNumber = do
    base <- parseNumericPrefix
    x <- case base of 
        'h' -> many1 hexDigit
        'o' -> many1 octDigit
        otherwise -> many1 digit
    notFollowedBy (char '.')
    return $ (Number . fst . head . (readBase base)) x
    where readBase 'd' = readDec
          readBase 'o' = readOct
          readBase 'h' = readHex

-- 3.3.6 (add Float parsing)
parseFloat :: Parser LispVal
parseFloat = do
    whole <- many1 digit
    char '.'
    part <- many1 digit
    exp <- option "0" (oneOf "eE" >> many1 digit)
    let num = whole ++ "." ++ part ++ "e" ++ exp
    return $ (Float . fst . head . readFloat) num

-- Ex. 3.3.7 add full numeric types
parseRational :: Parser LispVal
parseRational = do
    top <- many1 digit
    char '/'
    bot <- many1 digit
    return $ Ratio ((read top) % (read bot))

-- e.g. 103+2i
-- Ex. 3.3.7 add full numeric types
-- TODO fix support for decimals
parseComplex :: Parser LispVal
parseComplex = do
    real <- many1 digit
    char '+'
    imag <- many1 digit
    char 'i'
    return $ (Complex . Couble) ((read real) :+ (read imag))

-- Ex. 3.3.1.1 (parseNumber using do)
parseNumber' :: Parser LispVal
parseNumber' = do
    x <- many1 digit
    return $ (Number (read x))

-- Ex. 3.3.1.2 (parseNumber using bind)
parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>=
                \str -> return $ (Number . read) str

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr space

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

-- Ex. 3.4.1
parseBackquoted :: Parser LispVal
parseBackquoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

-- Ex. 3.4.1
-- handles , and ,@
parseUnquote :: Parser LispVal
parseUnquote = do
    style <- try (char ',' >> char '@') <|> char ','
    x <- parseExpr
    return $ List [Atom (descriptor style), x]
    where descriptor ',' = "unquote"
          descriptor '@' = "unquote-list"

parseBoolean :: Parser LispVal
parseBoolean = do
    char '#'
    x <- oneOf "tf"
    return $ Bool $ toBool x
    where toBool 't' = True
          toBool 'f' = False

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> try parseFloat -- try not optional, consumes digits
    <|> try parseComplex -- try not optional, consumes digits
    <|> try parseRational -- try not optional, consumes digits
    <|> try parseNumber -- try not optional (numbers can start with #, so can chars)
    <|> try parseBoolean -- try not optional, starts with # as well.
    <|> parseChar
    <|> parseQuoted
    <|> parseBackquoted
    <|> parseUnquote
    <|> do char '('
           x <- (try parseList) <|> parseDottedList
           char ')'
           return x

eval :: LispVal -> ThrowsError LispVal
eval val@(Float _) = return val
eval val@(Number _) = return val
eval val@(Ratio _) = return val
eval val@(Complex _) = return val
eval val@(String _) = return val
eval val@(Char _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
    case result of
            Bool False -> eval alt
            otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval bad = throwError $ BadSpecialForm "Unrecognized special form" bad

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              -- Ex. 4.3.1 (add primitive type checking). TODO ensure functions only accept 1 argument?
              ("string?", typePredicate isString),
              ("number?", typePredicate isNumber),
              ("bool?", typePredicate isBool),
              ("real?", typePredicate isReal),
              ("char?", typePredicate isChar),
              ("ratio?", typePredicate isRational),
              ("complex?", typePredicate isComplex),
              -- Ex. 4.3.3 (symbol? handling)
              ("symbol?", typePredicate isSymbol),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eqv?", eqv),
              ("eq?", eqv),
              ("equal?", equal)]
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool
typePredicate :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
typePredicate op [val] = return $ op val
typePredicate op vals = throwError $ NumArgs 1 vals

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _ = Bool False

isChar :: LispVal -> LispVal
isChar (Char _) = Bool True
isChar _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber (Float _) = Bool True
isNumber (Ratio _) = Bool True
isNumber (Complex _) = Bool True
isNumber _ = Bool False

isReal :: LispVal -> LispVal
isReal (Float _) = Bool True
isReal (Number _) = Bool True
isReal (Ratio _) = Bool True
isReal _ = Bool False

isRational :: LispVal -> LispVal
isRational (Ratio _) = Bool True
isRational (Number _) = Bool True
isRational _ = Bool False

isComplex :: LispVal -> LispVal
--isComplex (Float _) = Bool True
--isComplex (Number _) = Bool True
isComplex (Complex _) = Bool True
isComplex _ = Bool False

isSymbol :: LispVal -> LispVal
isSymbol (List [Atom "quote", List []]) = Bool False
isSymbol (List _) = Bool True
isSymbol (Atom _) = Bool True
isSymbol (DottedList _ _) = Bool True
isSymbol _ = Bool False

--sameConstructor :: LispVal -> [LispVal] -> LispVal
--sameConstructor a (b:_) | toConstr a == toConstr b = Bool True
--                        | otherwise                = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
{- Removed per Ex. 4.3.2
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
-}
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [DottedList [x] y] = return y
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(Float arg1), (Float arg2)] = return $ Bool $ arg1 == arg2
eqv [(Char arg1), (Char arg2)] = return $ Bool $ arg1 == arg2
eqv [(Ratio arg1), (Ratio arg2)] = return $ Bool $ arg1 == arg2
eqv [(Complex arg1), (Complex arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                                    (and $ map eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
    `catchError` (const $ return False)
    
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
