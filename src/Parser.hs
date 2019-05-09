module Parser ( nameParser
              , descParser
              , unreleasedVersionParser
              , diffEntryParser
              , entryParser
              , sectionNameParser
              , sectionParser
              , dateParser
              , versionParser
              , changelogParser
              , Parser
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

nameParser :: Parser String
nameParser = nameVia namePrefix

sectionNameParser :: Parser String
sectionNameParser = lexeme $ sectionPrefix *> choice (string <$> ["Changed", "Added", "Fixed", "Removed"])

descParser :: Parser String
descParser = lexeme $ manyTill (choice [printChar, newline]) (lookAhead versionPrefix)

unreleasedVersionParser :: Parser Version
unreleasedVersionParser = versionPrefix *> brackets (string "Unreleased") $> Unreleased

tagParser :: Parser String
tagParser = brackets (many $ choice [alphaNumChar, char '.'])

diffEntryParser :: Parser Diff
diffEntryParser = Diff <$> version <*> content 
  where version = tagParser <* symbol ":"
        content = contentTill eoi

entryParser :: Parser String
entryParser = entryPrefix *> contentTill eoi

sectionParser :: Parser Section
sectionParser = Section <$> sectionNameParser <*> manyTill entryParser eoi

dateParser :: Parser String
dateParser = lexeme $ dateConstruct <$> year <*> sep <*> month <*> sep <*> day
  where dateConstruct y s1 m s2 d = y ++ s1 ++ m ++ s2 ++ d
        year = count 4 digitChar
        month = count 2 digitChar
        day = count 2 digitChar
        sep = string "-"

versionParser :: Parser Version
versionParser = Version <$> tag <*> date <*> yanked <*> sections
  where tag = versionPrefix *> tagParser
        date = symbol "-" *> dateParser
        yanked = (brackets (string "YANKED") $> True) <|> return False
        sections = many sectionParser

changelogParser :: Parser Changelog
changelogParser = Changelog <$> nameParser <*> descParser <*> versionsParser <*> diffsParser
  where versionsParser = many $ try versionParser <|> unreleasedVersionParser
        diffsParser = many diffEntryParser
