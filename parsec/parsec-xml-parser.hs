{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative hiding ((<|>),many)
import Control.Monad
import Text.Parsec
import Text.Parsec.String


type AttrName = String
type AttrVal  = String
type Namespace = String
data Attribute = Attribute AttrName Namespace AttrVal deriving (Show) 

data XML =  Element String Namespace [Attribute] [XML]
          | SelfClosingTag String Namespace [Attribute]
          | Decl String
          | Body String 
        deriving (Show)


-- 
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces <* (many comment) <* spaces 

-- 
document :: Parser [XML]
document = do                   
  y <- spaces *> (lexeme $ try xmlDecl <|> element)
  x <- lexeme $ many element
  return (y : x)

-- 
xmlDecl ::Parser XML
xmlDecl = do 
  void $ string "<?xml" 
  decl <- many (noneOf "?>") 
  void $ string "?>"
  return (Decl decl)

-- 
comment :: Parser ()
comment = do 
  void $ (try (string "<!--"))
  void $ manyTill anyChar (try $ string "-->")

identifier = many (letter <|> digit <|> char '_')

element ::Parser XML
element = do
  void $ lexeme $ char '<'
  name <- lexeme $ identifier
  nmspace <- lexeme $ option "" namespace
  attr <- lexeme $ many attribute
  close <- lexeme $ try (string "/>" <|> string ">") 
  if (length close) == 2 then return (SelfClosingTag name nmspace attr)
                        else do 
                                elementBody <- many elementBody
                                void $ lexeme $ endTag (if nmspace == "" then  name else name ++ ":" ++ nmspace)
                                return (Element name nmspace attr elementBody)

-- парсинг тіла тегу
elementBody = spaces *> try element <|> text

-- парсинг закриваючої частини тегу
endTag str = string "</" *> string str <* char '>'

-- парсинг тексту в тілі тегу  
text = Body <$> many1 (noneOf "><")

-- парсинг неймспейсу  
namespace = do
  void $ char ':'
  name <- lexeme $ many1 $ noneOf " = />"
  return name

-- парсинг атрибуту
attribute = do
  name <- lexeme $ many $ noneOf ": = />" 
  nmspace <- lexeme $ option "" namespace
  void $ lexeme $ char '='
  void $ lexeme $ char '"'
  value <- lexeme $ many $ noneOf ['"']
  void $ lexeme $ char '"'
  return (Attribute name nmspace value)

-- 
extractBody (Element _ _ _ (b:_))  = (bodyString b)
  where bodyString (Body str) = str
extractBody _                    = ""

docFromFile filepath = do
  xml <- readFile filepath
  return (docFromString xml) 

docFromString xml = parse document "" xml 