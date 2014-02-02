
module Database.Distributed.Parser where


import Database.Distributed.Message
import Database.Distributed.Key
import Database.Distributed.Utility (white, red, reset)

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
insert = liftA2 (Insert . Key) (symbol "ins" *> unsigned_int) anyString

lookup :: Parser Message
lookup = liftA (Lookup . Key) (symbol "look" *> unsigned_int)

delete :: Parser Message
delete = liftA (Delete . Key) (symbol "del" *> unsigned_int)



center :: Parser ShowMessage
center = symbol "cen" >> return ShowCenter

stoMap :: Parser ShowMessage
stoMap = symbol "sto" >> return ShowStorageMap

proMap :: Parser ShowMessage
proMap = symbol "pro" >> return ShowProcessMap

showCmd :: Parser Message
showCmd = symbol "show" *> liftA SM (center <|> stoMap <|> proMap)

commands :: Parser [Message]
commands =
  many (whiteSpace *> (insert <|> lookup <|> delete <|> showCmd) <* char ';')


prompt :: Handle -> IO (Maybe [Message])
prompt hdl = do
  void $ hPrintf hdl (white ++ "$> " ++ reset)
  line <- hGetLine hdl
  putStrLn line
  case parse commands "command line" line of
       Left err ->
         hPrintf hdl (red ++ "parse error: " ++ show err ++ reset ++ "\n")
           >> return Nothing
       Right ms -> return (Just ms)