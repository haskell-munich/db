


module Parser where


import Message

import Text.ParserCombinators.Parsec

import Control.Applicative (liftA, liftA2, (<*), (*>))

import System.IO
  ( hGetLine, hSetNewlineMode, hSetBuffering,
    BufferMode(LineBuffering, NoBuffering), Handle,
    universalNewlineMode, stdout)

import Text.Printf (hPrintf, printf)

import Prelude hiding (lookup)

import qualified Data.Char as Char

symbol :: String -> Parser String
symbol name = lexeme (string name)

lexeme :: Parser a -> Parser a
lexeme = (<* whiteSpace)

whiteSpace :: Parser ()
whiteSpace = skipMany (satisfy Char.isSpace) <?> "whitespace"


unsigned_int :: Parser Int
unsigned_int = lexeme $ liftA read (many1 digit)

anyString :: Parser String
anyString = many (noneOf ";")


insert :: Parser Message
insert = liftA2 Insert (symbol "ins" *> unsigned_int) anyString

lookup :: Parser Message
lookup = liftA Lookup (symbol "look" *> unsigned_int)

delete :: Parser Message
delete = liftA Delete (symbol "del" *> unsigned_int)

commands :: Parser [Message]
commands =
  many (whiteSpace *> (insert <|> lookup <|> delete) <* char ';')


prompt :: Handle -> IO (Maybe [Message])
prompt hdl = do
  hPrintf hdl "$> "
  line <- hGetLine hdl
  putStrLn line
  case parse commands "command line" line of
       Left err -> 
         hPrintf hdl ("parse error: " ++ show err ++ "\n") >> return Nothing
       Right ms -> return (Just ms)