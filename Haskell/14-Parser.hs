import Data.Maybe
import Data.Bifunctor
import Data.Char

newtype Parser a = 
  Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  -- fmap f p = Parser $ fmap (bimap f id) . parse p
  fmap f = Parser . (fmap (bimap f id) . ) . parse
  
instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  pf <*> pa = Parser (\s ->
    case parse pf s of
      Nothing      -> Nothing
      Just (f, s') -> case parse pa s' of
        Nothing       -> Nothing
        Just (a, s'') -> Just (f a, s''))

-- Special case of sequenceA
seqA :: Applicative f => [f a] -> f [a]
seqA [] = pure []
seqA (fa : fas) = (:) <$> fa <*> seqA fas

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]  -- one or more
  some pa = (:) <$> pa <*> many pa
  many :: f a -> f [a]  -- zero or more

instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l

instance Alternative Parser where
  empty = Parser (\s -> Nothing)
  pa <|> pb = Parser (\s ->
    case parse pa s of
      Just as -> Just as
      Nothing -> parse pb s)
  {-
  -- Or, using the Alternative instance for Maybe:
  pa <|> pb = Parser (\s -> parse pa s <|> parse pb s)
  -}
  some pa = (:) <$> pa <*> many pa
  many pa = some pa <|> pure []
  many pa = some pa <|> pure []

-- Basic parsers

char :: Char -> Parser Char
char c = Parser parseC 
  where
    parseC [] = Nothing
    parseC (a : as) =
      if a == c 
      then Just (c, as)
      else Nothing

string :: String -> Parser String
string = seqA . fmap char
-- string [] = empty
-- string (c : cs) = (:) <$> char c <*> string cs

parens :: Parser a -> Parser a
parens a = char '(' *> a <* char ')'

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser sat
  where sat [] = Nothing
        sat (a : as) = if pred a
                       then Just (a, as)
                       else Nothing

digit' :: Parser Char
digit' = satisfy isDigit

digit :: Parser Char
digit = Parser (\s -> case s of
    (c:cs) | isDigit c -> Just (c, cs)
    _                  -> Nothing)

space :: Parser Char
space = char ' ' <|> char '\t' 

-- Skip space
ss :: Parser String
ss = many space

-- Lambda expression parser

type Name = Char

data Expr = 
    Var Name
  | Lambda Name Expr 
  | App Expr Expr
  deriving (Eq, Show)

-- Variable name is a lower case letter
name :: Parser Char
name = satisfy isLower

var :: Parser Expr
var = Var <$> name <* ss

lambda :: Parser Expr
lambda =
  Lambda <$> (char '\\' *> name <* char '.') <*> expr

term :: Parser Expr
term =  var
    <|> lambda
    <|> parens expr

-- Left associative function application or single term
expr :: Parser Expr
expr = foldl1 App <$> some term
  where foldl1 :: (a -> a -> a) -> [a] -> a
        foldl1 f [a] = a
        foldl1 f (a : as) = foldl f a as

testOR = parse expr "(\\p.\\q.p p q)(\\x.\\y.x)(\\x.\\y.y)"
