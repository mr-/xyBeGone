import           Text.Parsec hiding ((<|>), between)
import           Text.Parsec.Combinator hiding (between)
import           Text.Parsec.Char
import           Text.Parsec.Prim
import           Data.Maybe (catMaybes)

import           Control.Applicative hiding ((<|>), many)



type Curving = String
type Tip     = String
type Label   = String

data Arrow = Arrow {
   arrowCurving  :: Maybe Curving,
   arrowTip      :: Maybe Tip,
   arrowLabels   :: [Label] }
   deriving (Show)

(<||>) :: Maybe a -> Maybe a -> Maybe a
(Just x) <||> y = (Just x)
Nothing  <||> y = y

(<>) :: Arrow -> Arrow -> Arrow
(Arrow x y z)  <> (Arrow x' y' z') = Arrow (x <||> x') (y <||> y') (z ++ z')

emptyArrow = Arrow Nothing Nothing []




data LaTeX =
    LaText String
  | LaCommand String [Argument]
  | LaComment String
  | ArrowCommand Arrow
  deriving (Show)

data Argument = OptArg String
              | ReqArg String
              deriving (Show)




--comment = do{ string "%"
--              ; manyTill anyChar (try (string "-->"))
--            }

type Parser = Parsec String ()

text :: Parser LaTeX
text = LaText <$> manyTill anyChar (try isSpecial)

isSpecial = (oneOf "\\%" >> return ()) <|> eof

comment :: Parser LaTeX
comment = LaComment <$> do
                      { string "%"
                      ; manyTill anyChar (try eol)
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

parseArrow = undefined

parseTip = do
    { spaces
    ; char '@'
    ; t <- between '{' '}'
    ; return (emptyArrow { arrowTip = Just t })
    }

parseCurving = do
    { spaces
    ; char '@'
    ; t <- between '/' '/'
    ; return (emptyArrow { arrowCurving = Just t })
    }

parseLabels :: Parser Arrow
parseLabels = fmap (foldr (<>) emptyArrow) $ many parseLabel

parseLabel = do
    { spaces
    ; c <- oneOf "_^"
    ; l <- between '[' ']'
    ; return (emptyArrow {arrowLabels = [l]})
    }

foo = "\\ar @{-->} @/_2em/[ul]_{u} abc"


