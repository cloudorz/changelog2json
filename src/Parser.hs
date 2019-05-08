module Parser ( changelogName
              , changelogDesc
              , unreleasedVersion
              , diffRecord
              , changesEntry
              , sectionName
              ) where

import Data.Void
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types
import Control.Monad.Combinators

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets  = between (symbol "[") (symbol "]")

contentTill :: Parser a ->  Parser String
contentTill = manyTill printChar

namePrefix :: Parser ()
namePrefix = void $ symbol "# "

versionPrefix :: Parser ()
versionPrefix = void $ symbol "## "

sectionPrefix :: Parser ()
sectionPrefix = void $ symbol "### "

entryPrefix :: Parser ()
entryPrefix = void $ symbol "- "

eoi :: Parser ()
eoi = void eol <|> eof

nameVia :: Parser () -> Parser String
nameVia prefix = lexeme $ prefix *> contentTill eoi

changelogName :: Parser String
changelogName = nameVia namePrefix

sectionName :: Parser String
sectionName = nameVia sectionPrefix

changelogDesc :: Parser String
changelogDesc = lexeme $ manyTill (choice [printChar, newline]) (lookAhead versionPrefix)

unreleasedVersion :: Parser Version
unreleasedVersion = versionPrefix *> brackets (symbol "Unreleased") *> return Unreleased

diffRecord :: Parser Diff
diffRecord = Diff <$> version <*> content 
  where version = brackets (many (alphaNumChar <|> char '.')) <* symbol ":"
        content = contentTill eoi

changesEntry :: Parser String
changesEntry = entryPrefix *> contentTill eoi
