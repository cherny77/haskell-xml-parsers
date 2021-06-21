import Text.Regex.Applicative
import Data.Char

type AttrName = String
type AttrVal  = String
type Namespace = String
data Attribute = Attribute AttrName Namespace AttrVal deriving (Show) 

data XML =  Element String Namespace [Attribute] [XML]
          | Body String
          | Decl String 
        deriving (Show)

lexeme :: RE Char a -> RE Char a
lexeme re = spaces *> many comment *> spaces *> re <* spaces <* many comment <* spaces 

spaces :: RE Char [Char]
spaces = many (psym (\x -> (x ==' ') || (x =='\n')) )

xmlDecl :: RE Char XML
xmlDecl = Decl <$> delcParser
  where delcParser =  string "<?xml" *> many (anySym) <* string "?>"

document :: RE Char [XML]
document = (:) <$> decl <*> xmls <* spaces
  where
    decl = lexeme $ (xmlDecl <|>  Decl <$> spaces)
    xml = element ((element body) <|> body)
    xmls = (:) <$> xml <*> many empty

namespace :: RE Char [Char]
namespace = sym ':' *> many (psym (\x -> (x /= ' ') && (x /= '=')
                                                    && (x /= '/') 
                                                    && (x /= '>') ))

attribute :: RE Char Attribute           
attribute = Attribute <$> name <*> nmspace <*> value
  where 
    name = spaces *> many (psym (\x -> (x /= ':') && (x /= ' ') 
                                                  && (x /= '=')
                                                  && (x /= '/') 
                                                  && (x /= '>'))) 
    value = (many $ psym ( /= '"') ) <* sym '"' <* spaces
    nmspace = lexeme $ (namespace <|> spaces) <* spaces <* (lexeme $ sym '=') <* sym '"'

comment :: RE Char [Char]
comment = string "<!--" *> (many anySym) <* string "-->"

identifier = many (psym isLetter <|> psym isDigit <|> sym '_')

element :: RE Char XML -> RE Char XML
element bodyParser = Element <$> name <*> nmspace <*> attrs <*> (many  $ lexeme $ bodyParser) <* endTag
  where
    name = lexeme $ sym '<' *> (lexeme $ identifier)
    nmspace = lexeme (namespace <|> spaces)
    attrs = (many attribute) <* sym '>'
    endTag = string "</" *> (many $ psym ( /= '>')) <* sym '>' 

body::RE Char XML
body = Body <$> (spaces *> ((many $ lexeme $ psym (/= '<')) <|> spaces))

docFromFile filepath = do
  xml <- readFile filepath
  return (docFromString xml) 

docFromString xml = match (document) xml 

