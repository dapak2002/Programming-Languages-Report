{-# LANGUAGE 
    RankNTypes, RecursiveDo, RecordWildCards, TupleSections,
    OverloadedStrings, ExplicitForAll, TypeApplications, ScopedTypeVariables, 
    TypeFamilies #-}

module LamFunParser(parse, program_Grammar, expr_Grammar, unTok) where

import LamFunLexer
import LamFunSyntax

import Data.Text(Text)
import qualified Data.Text as T
import Text.Earley
import Text.Earley.Mixfix
import Control.Arrow(first)

import qualified Data.HashSet as HS
import qualified Data.Set as S
import Data.String(IsString(..))
import Data.String.Conv
import Data.Char(isDigit)
import Control.Applicative((<|>), empty, many, some)
import Data.Hashable(Hashable)
import Data.Singletons(SingI, sing)


type G t = forall r. Grammar r (Prod r (Token Text) (Token Text) t)

-- rule for a variable name, excluding the set of reserved names
var :: Prod r e (Token Text) (Token Text)
var = satisfy (\t -> 
    let str = unTok t
        head_letter = T.head str 
    in
        not (str `HS.member` (HS.fromList $ "rec" : "val" : "let" : "true": "false": "while" : "do" : reservedKeywords)) &&
        T.length str > 0 &&
        (T.length str == 1 || head_letter /= '-') &&
        not (isDigit head_letter))


bracketed x = namedToken "(" *> x <* namedToken ")"


expr_Grammar :: forall ver. SingI ver => G (Expr_ ver (Token Text))
expr_Grammar = mdo
    -- v
    varR <- rule $ Variable_ <$> (var <|> bracketed (satisfy (\t -> True)))

    -- \ v1 ... vn . e
    lambdaR <- rule $ foldLam_ <$> (namedToken "\\" *> some var <* namedToken ".") <*> expr

    -- e e'
    -- e infixOp e'
    atom <- rule $ bracketed expr <|> varR <|> numberR <|> booleanR <|> strLitR <|> 
        consR <|> caseR <|> contentsR
    normalAppR <- rule $ atom <|> App_ <$> normalAppR <*> atom
    appR <- mixfixExpression identTable normalAppR mkApp_

    -- 0 1 -1 ...
    numberR <- rule $ case v of
        SLamCBN -> empty
        SLamCBV -> empty
        _ -> unsafeMkNumber_ <$> ((read . T.unpack . unTok) <$> 
                satisfy (\Token{..} -> 
                    T.length unTok > 0 &&
                    T.all isDigit unTok))

    -- true 
    -- false
    booleanR <- rule $ case v of
            SLamCBN -> empty
            SLamCBV -> empty
            _ -> 
                    (pure $ unsafeMkBoolean_ True) <$> namedToken "true" 
                <|> (pure $ unsafeMkBoolean_ False) <$> namedToken "false"

    -- [e1 , ..., en]
    -- []
    -- e : e'
    listR <- rule $ 
            (:[]) <$> expr
        <|> (:) <$> (expr <* namedToken ",") <*> listR
    consAuxR <- rule $
                pure unsafeMkNil_ <$> (namedToken "[" *> namedToken "]")
            <|> unsafeMkCons_ <$> (atom <* namedToken ":") <*> consR
            <|> unsafeMkCons_ <$> (atom <* namedToken ":") <*> atom
            <|> (foldr unsafeMkCons_ unsafeMkNil_) <$> (namedToken "[" *> listR <* namedToken "]")
    consR <- rule $ case v of
            SLamCBN -> empty
            SLamCBV -> empty
            _ -> consAuxR 
    
    -- case e of { e1 -> e1' ; en -> e' ; _ -> em' }
    caseAuxR <- rule $
                (\b -> [(Nothing,b)]) <$> (namedToken "_" *> namedToken "->" *> expr)
            <|> (\a b -> [(Just a,b)]) <$> expr <*> (namedToken "->" *> expr)
            <|> (\b xs -> (Nothing,b):xs) <$> (namedToken "_" *> namedToken "->" *> expr <* namedToken ",") <*> caseAuxR
            <|> (\a b xs -> (Just a,b):xs) <$> expr <*> (namedToken "->" *> expr <* namedToken ",") <*> caseAuxR
    caseR <- rule $ case v of
            SLamCBN -> empty
            SLamCBV -> empty
            SLamNat -> empty
            _ -> unsafeMkCase_ <$> (namedToken "case" *> expr <* namedToken "of") <*> (namedToken "{" *> caseAuxR <* namedToken "}")

    -- let val v = e in e'
    -- let rec v v1 ... vn = e in e'
    valR <- rule $ Val_ <$> (namedToken "val" *> var <* namedToken "=") <*> expr
    recR <- rule $ case v of
            SLamCBN -> empty
            SLamCBV -> empty
            SLamNat -> empty
            _ -> (\vs e -> case vs of
                [n] -> unsafeMkRec_ n e
                n:xs -> unsafeMkRec_ n (foldLam_ xs e))
                <$> (namedToken "rec" *> some var <* namedToken "=") <*> expr
    letR <- rule $ case v of
            SLamCBN -> empty
            SLamCBV -> empty
            SLamNat -> empty
            _ -> unsafeMkLet_ <$> (namedToken "let" *> (valR <|> recR) <* namedToken "in") <*> expr

    -- "string"
    strLitR <- rule $ case v of
            SLamCBN -> empty
            SLamCBV -> empty
            SLamNat -> empty
            SLamRec -> empty
            _ -> unsafeMkStrLit_ <$> (namedToken "\"" *> satisfy (\_ -> True) <* namedToken "\"")
    
    -- !e dereference operator
    contentsR <- rule $ case v of
            SLamCBN -> empty
            SLamCBV -> empty
            SLamNat -> empty
            SLamRec -> empty
            _ -> unsafeMkContents_ <$> (namedToken "!" *> atom)
    -- e
    expr <- rule $ appR <|> lambdaR <|> letR
    return expr
    where
        v = sing @ver 

        infixL a = ([Nothing, Just (namedToken a), Nothing], LeftAssoc)
        infixR a = ([Nothing, Just (namedToken a), Nothing], RightAssoc)
        infixN a = ([Nothing, Just (namedToken a), Nothing], NonAssoc)
        prefix a = ([Just (namedToken a), Nothing], NonAssoc)
        while = ([Just (namedToken ("while" :: Token Text)), Nothing, Just (namedToken "do"), Nothing], NonAssoc)

        -- each sublist represents a binding group, i.e. == and != bind at the same level
        -- the syntax at the top of the list binds more losely than stuff at the end.
        identTable =
          [ case v of { SLamMem -> [infixR ";"] ; SLamArray -> [infixR ";"] ; _ -> []}
          , case v of { SLamMem -> [while] ; SLamArray -> [while] ; _ -> []}
          , case v of { SLamMem -> [infixL ":="] ; SLamArray -> [infixL ":="] ; _ -> []}
          , [infixN "==", infixN "!="]
          , [infixN ">", infixN ">=", infixN "<", infixN "<="]
          , [infixL "+", infixL "-"]
          , [infixL "*", infixL "/"]
          , [prefix "-"] 
          ]

        mkApp_ :: SingI v => Holey (Token Text) -> [Expr_ v (Token Text)] -> Expr_ v (Token Text)
        -- hack for `-a` ... we translate to `~ a` to disambiguate from the binary `-`
        mkApp_ [Just (Token "-" rs re cs ce),Nothing] [a] = App_ (Variable_ (Token "~" rs re cs ce)) a
        mkApp_ [Nothing,Just (Token ";" _ _ _ _),Nothing] [a,b] = unsafeMkSequence_ a b
        mkApp_ [Nothing,Just (Token ":=" _ _ _ _),Nothing] [a,b] = unsafeMkAssign_ a b
        mkApp_ [Just (Token "while" _ _ _ _),Nothing, Just (Token "do" _ _ _ _),Nothing] [e,b] = unsafeMkWhile_ e b
        mkApp_ [Nothing,Just op,Nothing] [a,b] = App_ (App_ (Variable_ op) a) b


foldLam_ :: [a] -> Expr_ v a -> Expr_ v a
foldLam_ [x] e = Lambda_ x e
foldLam_ (x:xs) e = Lambda_ x $ foldLam_ xs e


defn_Grammar :: forall ver. SingI ver => G (Defn_ ver (Token Text))
defn_Grammar = do
    expr <- expr_Grammar
    valR <- rule $ Val_ <$> (namedToken "val" *> var <* namedToken "=") <*> expr
    recR <- rule $ case sing @ver of
        SLamCBN -> empty
        SLamCBV -> empty
        SLamNat -> empty
        _ -> (\vs e -> case vs of
            [n] -> unsafeMkRec_ n e
            n:xs -> unsafeMkRec_ n (foldLam_ xs e))
            <$> (namedToken "rec" *> some var <* namedToken "=") <*> expr
    return $ valR <|> recR


program_Grammar :: forall ver. SingI ver => G [Program_ ver (Token Text)]
program_Grammar = do
    expr <- expr_Grammar
    defn <- defn_Grammar
    return $ many $ case sing @ver of 
        SLamCBN -> Calculate_ <$> expr <* namedToken ";;"
        SLamCBV -> Calculate_ <$> expr <* namedToken ";;"
        _ ->    (Calculate_ <$> expr <* namedToken ";;") 
            <|> (unsafeMkDefine_ <$> defn <* namedToken ";;")


parse :: (Eq t, Ord t) => G t -> (t -> t') -> Text -> Either (Report Text [Text]) t'
parse grammar mod t =
    case fullParses (parser $ grammar) $ tokenize defaultPosition t of
        (x@(p:_) , _) | S.size (S.fromList x) == 1 -> Right (mod p)
        -- (x@(p:_) , _) | otherwise -> ambiguous parse
        (_ , (Report p e u)) -> Left $ Report p (map unTok e) (map unTok u)
