{-# LANGUAGE 
    DerivingStrategies, RecordWildCards, ViewPatterns, 
    FlexibleInstances, FlexibleContexts,
    MultiParamTypeClasses, GeneralizedNewtypeDeriving,
    DeriveFunctor, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}


module LamFunLexer(Token(..), reservedKeywords, tokenize, defaultPosition, Row(..), Col(..)) where

import Data.String(IsString(..))
import Data.String.Conv
import Data.Text(Text)
import qualified Data.Text as T
import Control.Monad.State
import Data.Bifunctor(bimap)
import Control.Arrow(first,second)
import Data.Maybe(fromJust)
import Data.List(sortBy)
import GHC.Generics(Generic)
import Data.Hashable(Hashable)

prefix :: String -> String -> Maybe String
prefix [] ys = Just ys
prefix (_:_) [] = Nothing
prefix (x:xs) (y:ys) | x == y = prefix xs ys
                     | otherwise = Nothing

newtype Row = Row Int 
    deriving newtype (Eq, Show, Num, Hashable)

newtype Col = Col Int 
    deriving newtype (Eq, Show, Num, Hashable)


defaultPosition :: (Row, Col)
defaultPosition = (Row 0, Col 0)


data Token a = Token {
    unTok :: a,
    rowStart :: Row,
    rowEnd :: Row,
    colStart :: Col,
    colEnd :: Col
} deriving (Generic, Hashable)



tok :: a -> Token a
tok a = Token a (-1) (-1) (-1) (-1)


instance Show a => Show (Token a) where
    -- show = show . unTok
    show Token{..} 
        | rowStart == -1 || 
          rowEnd == -1   || 
          colStart == -1 ||
          colEnd == -1   = show unTok
        | otherwise = show unTok ++ " : Row (" ++ show rowStart ++ ":" ++ show rowEnd ++ "), Col (" ++ show colStart ++ ":" ++ show colEnd ++ ")"


instance Eq a => Eq (Token a) where
    t1 == t2 = unTok t1 == unTok t2

instance Ord a => Ord (Token a) where
    compare t1 t2 = compare (unTok t1) (unTok t2)

instance IsString a => IsString (Token a) where
    fromString s = Token (fromString s) 0 0 0 0

instance StringConv a b => StringConv (Token a) b where
    strConv l = strConv l . unTok



joinT :: Monoid a => Token a -> Token a -> Token a
joinT (Token t1 rS _ cS _) (Token t2 _ rE _ cE) = Token (t1 <> t2) rS rE cS cE



incrBy :: MonadState (Row, Col) m => Text -> m ()
incrBy t | T.null t = return ()
incrBy t | "\n" `T.isPrefixOf` t = do
    modify (bimap (+1) (const 1))
    incrBy $ T.tail t
incrBy t = do
    modify (second (+1))
    incrBy $ T.tail t

data DropOrKeepLabel = Drop | Keep | New deriving (Show, Eq)

data DropOrKeep a = DropOrKeep {
    label :: DropOrKeepLabel
  , content :: a
  } deriving (Show, Functor)

type TokenizerSettingsText = [(Text,Text -> ([DropOrKeep Text],Text))]

mkTokens :: MonadState (Row, Col) m => [DropOrKeep Text] -> m [DropOrKeep (Token Text)]
mkTokens [] = pure []
mkTokens (DropOrKeep l x:xs) = do
    (rowStart,colStart) <- get
    incrBy x
    (rowEnd,colEnd) <- get
    let token = Token x rowStart rowEnd colStart colEnd
    (DropOrKeep l token:) <$> mkTokens xs


startsWith :: TokenizerSettingsText -> Text -> Maybe ([DropOrKeep Text],Text)
startsWith [] str = Nothing
startsWith ((p,f):xs) str | p `T.isPrefixOf` str = Just $ f str
                              | otherwise = startsWith xs str


tokenizer :: MonadState (Row, Col) m => TokenizerSettingsText -> Text -> m [DropOrKeep (Token Text)]
tokenizer _  t | T.null t = return []
tokenizer ts (startsWith ts -> Just (potentialTokens, rest)) = do
    toks <- mkTokens potentialTokens
    (toks ++) <$> tokenizer ts rest
tokenizer ts t = do
    (rowStart,colStart) <- get
    incrBy $ T.singleton $ T.head t
    (rowEnd,colEnd) <- get
    let token = Token (T.singleton $ T.head t) rowStart rowEnd colStart colEnd
    tokens <- tokenizer ts $ T.tail t
    case tokens of
        [] -> return [DropOrKeep Keep token]
        (DropOrKeep Keep x:xs) -> return $ DropOrKeep Keep (joinT token x) : xs
        (x:xs) -> return $ DropOrKeep Keep token : x : xs


whitespace :: (Text, Text -> ([DropOrKeep Text],Text))
whitespace = (" ", f)
    where
        f :: Text -> ([DropOrKeep Text],Text)
        f x = ([DropOrKeep Drop $ T.takeWhile (==' ') x], T.dropWhile (==' ') x)

newline :: (Text, Text -> ([DropOrKeep Text],Text))
newline = ("\n", f)
    where
        f :: Text -> ([DropOrKeep Text],Text)
        f x = ([DropOrKeep Drop $ T.takeWhile (=='\n') x], T.dropWhile (=='\n') x)

tab :: (Text, Text -> ([DropOrKeep Text],Text))
tab = ("\t", f)
    where
        f :: Text -> ([DropOrKeep Text],Text)
        f x = ([DropOrKeep Drop $ T.takeWhile (=='\t') x], T.dropWhile (=='\t') x)


reservedKeyword :: Text -> (Text, Text -> ([DropOrKeep Text],Text))
reservedKeyword w = (w, f)
    where
        f :: Text -> ([DropOrKeep Text],Text)
        f x = ([DropOrKeep New w], fromJust $ T.stripPrefix w x)


block :: Text -> Text -> Char -> (Text, Text -> ([DropOrKeep Text],Text))
block start end escape = (start, f)
    where
        f:: Text -> ([DropOrKeep Text],Text)
        f x = 
            -- if we find the cloing block `end` then we add start and end as Tokens and take the string inbetween
            if end `T.isPrefixOf` rest' then
                ([DropOrKeep New start, DropOrKeep New quotePrefix, DropOrKeep New end], rest)
            -- if we can't find the closing `end` tag, we break on the first occurence of space/tab/newline
            else 
                ([DropOrKeep New quotePrefixAlt],restAlt)
            where
                (quotePrefix, rest') = breakOn $ T.drop (T.length start) x
                rest = T.drop (T.length end) rest'

                quotePrefixAlt = T.takeWhile (\c -> not (c == ' ' || c == '\t' || c == '\n')) x
                restAlt = T.dropWhile (\c -> not (c == ' ' || c == '\t' || c == '\n')) x

                breakOn str | T.null str = (T.empty, T.empty)
                            | otherwise = 
                    let (a,b) = T.breakOn end str in
                        if not (T.null a) && T.last a == escape then 
                            let (a',b') = breakOn (T.tail b) in 
                                (T.append a (T.cons escape $ T.append end a'), b')
                        else (a,b)

blockDrop :: Text -> Text -> (Text, Text -> ([DropOrKeep Text],Text))
blockDrop start end = (start, f)
    where
        f:: Text -> ([DropOrKeep Text],Text)
        f x = ([DropOrKeep Drop start, DropOrKeep Drop quotePrefix, DropOrKeep Drop end], rest)
            where
                (quotePrefix, rest') = T.breakOn end $ T.drop (T.length start) x
                rest = T.drop (T.length end) rest'


quotes = block "\"" "\"" '\\' -- quoteEscape

ignoreComment = blockDrop "--" "\n"
ignoreComment2 = blockDrop "{-" "-}"


longestFirst :: Text -> Text -> Ordering
longestFirst a b = case compare (T.length a) (T.length b) of
    EQ -> compare a b
    LT -> GT
    GT -> LT


tokenize :: (Row,Col) -> Text -> [Token Text]
tokenize start_loc = 
    map content .
    filter ((/= Drop) . label) . 
    flip evalState start_loc . 
    tokenizer (
        whitespace : newline : tab : 
        quotes :
        ignoreComment : ignoreComment2 : 
        map reservedKeyword reserved
    )
    where
        reserved = sortBy longestFirst $ reservedKeywords


reservedKeywords :: [Text]
reservedKeywords = 
    ["\"", "(", ")", "[", "]", "{", "}", "->", ",", "=", ":=", "_", 
     "\\", ".", ";;", ";", ":", "&", "+", "-", "*", "/", "<", "<=", 
     ">", ">=", "==", "!=", "!"]
