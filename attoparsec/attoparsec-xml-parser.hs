{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative 
import Control.Monad
import qualified Data.Attoparsec.Text as A 
import qualified Data.Text as T


type AttrName = String
type AttrVal  = String
type Namespace = String
data Attribute = Attribute AttrName Namespace AttrVal deriving (Show) 

data XML =  Element String Namespace [Attribute] [XML]
          | SelfClosingTag String Namespace [Attribute]
          | Decl String
          | Body String 
        deriving (Show)



spaces = A.many' $ A.space
lexeme p = p <* spaces <* (A.many' comment) <* spaces 

noneOf cs = A.satisfy (\c -> and [ c /= x | x <- cs])

document = do                   
  y <- spaces *> (lexeme $ xmlDecl <|> element)
  x <- lexeme $ A.many' element
  return (y : x)

identifier = A.many' (A.letter <|> A.digit <|> A.char '_')

xmlDecl = do 
  void $ A.string "<?xml" 
  decl <- A.many' (noneOf "?>") 
  void $ A.string "?>"
  return (Decl decl)

comment = do 
  void $ (A.string "<!--")
  void $ A.manyTill A.anyChar (A.string "-->")

element = do
  void $ lexeme $ A.char '<'
  name <- lexeme $ identifier
  nmspace <- lexeme $ A.option "" namespace
  attr <- lexeme $ A.many' attribute
  close <- lexeme (A.string "/>" <|> A.string ">") 
  if (T.length close) == 2 then return (SelfClosingTag name nmspace attr)
                        else do 
                                elementBody <- A.many' elementBody
                                void $ lexeme $ endTag (if nmspace == "" then T.pack name else T.pack (name ++ ":" ++ nmspace))
                                return (Element name nmspace attr elementBody)

-- парсинг тіла тегу
elementBody = spaces *> element <|> text

-- парсинг закриваючої частини тегу
endTag str = A.string "</" *> A.string str <* A.char '>'

-- парсинг тексту в тілі тегу  
text = Body <$> A.many1 (noneOf "><")

-- парсинг неймспейсу  
namespace = do
  void $ A.char ':'
  name <- lexeme $ A.many1 $ noneOf " = />"
  return name

-- парсинг атрибуту
attribute = do
  name <- lexeme $ A.many' $ noneOf ": = />" 
  nmspace <- lexeme $ A.option "" namespace
  void $ lexeme $ A.char '='
  void $ lexeme $ A.char '"'
  value <- lexeme $ A.many' $ noneOf ['"']
  void $ lexeme $ A.char '"'
  return (Attribute name nmspace value)

extractBody :: XML -> String
extractBody (Element _ _ _ (b:_))  = (bodyString b)
  where bodyString (Body str) = str
extractBody _                    = ""

docFromFile :: FilePath -> IO (Either String [XML])
docFromFile filepath = do
  xml <- readFile filepath
  return (docFromString xml) 

-- docFromString xml = A.feed (A.parse document (T.pack xml)) T.empty

docFromString :: String -> Either String [XML]
docFromString xml = A.parseOnly document (T.pack xml)

