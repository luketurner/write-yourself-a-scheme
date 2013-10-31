module Main where

import System.Environment
import Numeric
import Monad
import Ratio
import Complex
import Text.ParserCombinators.Parsec hiding (spaces)

-- PROBLEMS
-- 3.1 - n/a
-- 3.2 - n/a
-- 3.3 - done
-- 3.4 - 2, 3
-- 4.1-3 - n/a
-- 4.4 - done (TODO revisit type-testing and symbol handling later)

data Couble = Couble (Complex Double) deriving (Show)

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
             deriving (Show)

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List l) = "(" ++ (unwords . map showVal) l ++ ")"
showVal (DottedList list tail) = "(" ++ (unwords . map showVal) list ++ " . " ++ showVal tail ++ ")"
showVal c = show c

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

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Float _) = val
eval val@(Char _) = val
eval val@(Ratio _) = val
eval val@(Complex _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              -- Ex. 4.3.1 (add primitive type checking). TODO ensure functions only accept 1 argument?
              ("string?", isString . head),
              ("number?", isNumber . head),
              ("bool?", isBool . head),
              ("real?", isReal . head),
              ("char?", isChar . head),
              ("ratio?", isRational . head),
              ("complex?", isComplex . head),
              -- Ex. 4.3.3 (symbol? handling)
              ("symbol?", isSymbol . head)] 

--tossTail :: LispVal -> LispVal -> [LispVal] -> LispVal 
--tossTail f (head:_) = f head

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
isSymbol (List _) = Bool True
isSymbol (Atom _) = Bool True
isSymbol (DottedList _ _) = Bool True

--sameConstructor :: LispVal -> [LispVal] -> LispVal
--sameConstructor a (b:_) | toConstr a == toConstr b = Bool True
--                        | otherwise                = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params


unpackNum :: LispVal -> Integer
unpackNum (Number n) = n

{- Removed per Ex. 4.3.2
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                            then 0
                            else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum  n
-}
unpackNum _ = 0

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)
