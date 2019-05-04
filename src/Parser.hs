module Parser where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types
import Control.Monad.Combinators

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets  = between (symbol "[") (symbol "]")

contentTill :: Parser a ->  Parser String
contentTill = manyTill printChar

parseChangelogName :: Parser String
parseChangelogName = symbol "# " *> contentTill eol
