
module Database.Distributed.Parser where


import Database.Distributed.Message

import Text.ParserCombinators.Parsec

import System.IO (hGetLine, Handle)

import Text.Printf (hPrintf)

import qualified Data.Char as Char

import Control.Applicative (liftA, liftA2, (<*), (*>))
import Control.Monad (void)

import Prelude hiding (lookup)


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
  void $ hPrintf hdl "$> "
  line <- hGetLine hdl
  putStrLn line
  case parse commands "command line" line of
       Left err -> 
         hPrintf hdl ("parse error: " ++ show err ++ "\n") >> return Nothing
       Right ms -> return (Just ms)