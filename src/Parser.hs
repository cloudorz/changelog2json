module Parser ( changelogName
              , changelogDesc
              , unreleasedVersion
              , diffRecord
              , changesEntry
              , sectionName
              , section
              , dateString
              , versionItem
              , changelogParser
              ) where

import Data.Void
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types
import Control.Monad.Combinators
import Data.Functor

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
sectionName = lexeme $ sectionPrefix *> choice (string <$> ["Changed", "Added", "Fixed", "Removed"])

changelogDesc :: Parser String
changelogDesc = lexeme $ manyTill (choice [printChar, newline]) (lookAhead versionPrefix)

unreleasedVersion :: Parser Version
unreleasedVersion = versionPrefix *> brackets (string "Unreleased") $> Unreleased

verString :: Parser String
verString = brackets (many $ choice [alphaNumChar, char '.'])

diffRecord :: Parser Diff
diffRecord = Diff <$> version <*> content 
  where version = verString <* symbol ":"
        content = contentTill eoi

changesEntry :: Parser String
changesEntry = entryPrefix *> contentTill eoi

section :: Parser Section
section = Section <$> sectionName <*> manyTill changesEntry eoi

dateString :: Parser String
dateString = lexeme $ dateConstruct <$> year <*> sep <*> month <*> sep <*> day
  where dateConstruct y s1 m s2 d = y ++ s1 ++ m ++ s2 ++ d
        year = count 4 digitChar
        month = count 2 digitChar
        day = count 2 digitChar
        sep = string "-"

versionItem :: Parser Version
versionItem = Version <$> tag <*> date <*> yanked <*> sections
  where tag = versionPrefix *> verString
        date = symbol "-" *> dateString
        yanked = (brackets (string "YANKED") $> True) <|> return False
        sections = many section

changelogParser :: Parser Changelog
changelogParser = Changelog <$> changelogName <*> changelogDesc <*> versionsParser <*> diffsParser
  where versionsParser = many $ (try versionItem) <|> unreleasedVersion
        diffsParser = many diffRecord
