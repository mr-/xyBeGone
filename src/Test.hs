import Text.Parsec hiding ((<|>), between)
import Text.Parsec.Combinator hiding (between)
import Text.Parsec.Char
import Text.Parsec.Prim
import Data.Maybe (catMaybes)
import Data.Foldable (foldMap)

import Control.Applicative hiding ((<|>), many)
import Data.Monoid

-- parseTest parseTex "abc\\xy%z"
-- [LaText "abc",LaCommand "xy" [],LaComment "z"]

data LaTeX =
    LaText String
  | ArrowCommand Arrow
  | LaCommand String [Argument]
  | LaComment String
  deriving (Show)

data Argument = OptArg String
              | ReqArg String
              deriving (Show)


data Arrow = Arrow {
   arrowDirection :: Maybe Direction,
   arrowCurving   :: Maybe Curving,
   arrowTip       :: Maybe Tip,
   arrowLabels    :: [Label] }
   deriving (Show)
type Direction = String
type Curving   = String
type Tip       = String
type Label     = LaTeX

(<||>) :: Maybe a -> Maybe a -> Maybe a
(Just x) <||> y = (Just x)
Nothing  <||> y = y


emptyArrow = Arrow Nothing Nothing Nothing []

instance Monoid Arrow where
  (Arrow d x y z)  `mappend` (Arrow d' x' y' z') = Arrow (d <||> d') (x <||> x') (y <||> y') (z ++ z')
  mempty  = emptyArrow




--comment = do{ string "%"
--              ; manyTill anyChar (try (string "-->"))
--            }

type Parser = Parsec String ()

parseTex :: Parser [LaTeX]
parseTex = manyTill parseTex' eof

parseTex' :: Parser LaTeX
parseTex' = do parseArrow
           <|> command
           <|> comment
           <|> text

text :: Parser LaTeX
text = LaText <$> manyTill anyChar (try $ lookAhead isSpecial)

isSpecial = (oneOf "\\%" >> return ()) <|> eof

comment :: Parser LaTeX
comment = LaComment <$> do
                      { string "%"
                      ; manyTill anyChar (try $ lookAhead eol)
                      }
eol = (newline >> return ()) <|> eof

command :: Parser LaTeX
command = do
                      { string "\\"
                      ; cmd  <- many1 letter
                      ; args <- parseArgs
                      ; return $ LaCommand cmd args
                      }

parseArgs :: Parser [Argument]
parseArgs = do
                      {
                      ; spaces
                      ; oArg <- optionMaybe optArg
                      ; spaces
                      ; rArg <- optionMaybe reqArg
                      ; return (catMaybes [oArg, rArg])
                      }
optArg:: Parser Argument
optArg = OptArg <$> between '[' ']'
reqArg = ReqArg <$> between '{' '}'

between :: Char -> Char -> Parser String
between a b = do char a
                 manyTill anyChar (try (char b))

parseArrow :: Parser LaTeX
parseArrow = do
            string "\\ar"
            args <- sequence [optionMaybe parseTip,
                             optionMaybe parseCurving]
            opt  <- between '[' ']'
            spaces
            mls  <- optionMaybe parseLabels
            let l = catMaybes (Just (emptyArrow {arrowDirection = Just opt}):mls:args)
            return (ArrowCommand $  mconcat l)

parseTip :: Parser Arrow
parseTip = do
    { spaces
    ; char '@'
    ; t <- between '{' '}'
    ; return (emptyArrow { arrowTip = Just t })
    }
parseCurving :: Parser Arrow
parseCurving = do
    { spaces
    ; char '@'
    ; t <- between '/' '/'
    ; return (emptyArrow { arrowCurving = Just t })
    }

parseLabels :: Parser Arrow
parseLabels = fmap (foldr (<>) emptyArrow) $ (singleton <$> parseLabel)

parseLabel = try parseBracedLabel  <|> try parseUnBracedLabel


parseUnBracedLabel = do
    { spaces
    ; c <- oneOf "_^"
    ; l <- parseToken
    ; return (emptyArrow {arrowLabels = [l]})
    }

parseToken :: Parser LaTeX
parseToken =  try command
          <|> LaText <$> (singleton <$> anyChar)
          where
            foo = (oneOf "\\" >> return ()) <|> (space >> return ()) <|> (eol >> return())


singleton x = [x]

parseBracedLabel = do
    { spaces
    ; c <- oneOf "_^"
    ; l <- sthBetween parseTex' '{' '}'
    ; return (emptyArrow {arrowLabels = l})
    }

sthBetween :: Parser a -> Char -> Char -> Parser [a]
sthBetween f a b = do char a
                      manyTill f (try (char b))


foo = "\\ar @{-->} @/_2em/[ul]_{u} abc"


