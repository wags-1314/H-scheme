module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

data SchemeObject = Atom String
             | List [SchemeObject]
             | DottedList [SchemeObject] SchemeObject
             | Number Integer
             | String String
             | Bool Bool
             | Character Char

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

whitespace :: Parser ()
whitespace = skipMany1 space

escapedChars :: Parser Char
escapedChars = do
    char '\\'
    ch <- oneOf "\\\"rnt"
    return $ case ch of
               '\\' -> ch
               '"' -> ch
               'n' -> '\n'
               'r' -> '\r'
               't' -> '\t'

parseString :: Parser SchemeObject
parseString = do
    char '"'
    x <- many (noneOf "\"\\" <|> escapedChars)
    char '"'
    return $ String x

parseBool :: Parser SchemeObject
parseBool = do
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseAtom :: Parser SchemeObject
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom (first : rest)

parseHex :: Parser SchemeObject
parseHex = do
    char 'x'
    x <- many1 hexDigit
    return $ Number $ fst (readHex x !! 0)

parseOct :: Parser SchemeObject
parseOct = do
    char 'o'
    x <- many1 octDigit
    return $ Number $ fst (readOct x !! 0)

parseDecimal :: Parser SchemeObject
parseDecimal = do
    char 'd'
    x <- many1 digit
    return $ Number $ read x

parseNumber :: Parser SchemeObject
parseNumber = ((many1 digit) >>= (return . Number . read))

-- parseCharacter :: Parser SchemeObject
-- parseCharacter = do


parseBang :: Parser SchemeObject
parseBang = char '#' >> (parseHex <|> parseOct <|> parseBool <|> parseDecimal)


parseExpr :: Parser SchemeObject
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseBang

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ (case val of
                                       Atom str -> ("atom: " ++ str)
                                       List _ -> "list"
                                       DottedList _ _ -> "dotted list"
                                       Number int -> ("int: " ++ (show int))
                                       String str -> ("str: " ++ str)
                                       Bool bool -> ("bool: " ++ (show bool)))

main :: IO()
main = do
    expr:_ <- getArgs
    putStrLn (readExpr expr)

