{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Exercises.Ch2.Regex where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Foldable        (asum)
import           Data.Maybe           (listToMaybe)

data Regex a
    = Epsilon
    | Char a
    | Regex a :.: Regex a
    | Regex a :|: Regex a
    | Star (Regex a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Regex where
    pure = Char
    Char f <*> r = f <$> r
    Epsilon <*> _ = Epsilon
    r1 <*> r2 =
        case r1 of
            a :.: b ->
                (a <*> r2) :.: (b <*> r2)
            a :|: b ->
                (a <*> r2) :|: (b <*> r2)
            Star r ->
                Star (r <*> r2)

instance Alternative Regex where
    empty = Epsilon
    (<|>) = (:|:)

renderRegex :: Regex Char -> String
renderRegex = \case
    Epsilon -> []
    Char a -> [a]
    a :.: b -> renderRegex a ++ renderRegex b
    a :|: b -> mconcat ["(", renderRegex a, "|", renderRegex b, ")"]
    Star r -> renderRegex r ++ "*"

simplify :: Regex a -> Regex a
simplify = \case
    -- leaves
    Epsilon -> Epsilon
    Char a -> Char a

    -- epsilon
    Epsilon :.: r -> simplify r
    r :.: Epsilon -> simplify r
    Epsilon :|: r -> simplify r
    r :|: Epsilon -> simplify r

    r1 :|: r2 -> simplify r1 :|: simplify r2
    r1 :.: r2 -> simplify r1 :|: simplify r2
    Star r -> Star (simplify r)


string :: [token] -> Regex token
string []     = Epsilon
string (x:xs) = foldr1 (:.:) (fmap Char (x:xs))

plus :: Regex a -> Regex a
plus reg = reg :.: Star reg

repeatR :: Int -> Regex a -> Regex a
repeatR i r
    | i <= 0 = Epsilon
    | otherwise = foldr1 (\_ acc -> r :.: acc) (replicate i Epsilon)

range :: (Enum a) => a -> a -> Regex a
range a z = asum (fmap pure [a .. z])

data RegexMatchFail tok
    = ExpectedCharButGot tok tok [tok]
    | UnexpectedEndOfInput
    deriving Show

match :: Eq t => Regex t -> StateT [t] (Except (RegexMatchFail t)) [t]
match r =
    case r of
        Epsilon ->
            pure []
        Char a -> do
            str <- get
            case str of
                [] -> throwError UnexpectedEndOfInput
                c:cs -> do
                    unless (c == a)
                        (throwError (ExpectedCharButGot a c cs))
                    put cs
                    pure [c]
        Star r' -> do
            x <- match r' `catchError` \_ -> pure []
            case x of
                [] -> pure []
                _  -> match (Star r')

        r1 :.: r2 -> do
            as <- match r1
            bs <- match r2
            pure (as ++ bs)

        r1 :|: r2 ->
            match r1 `catchError` \_ ->
                match r2

runMatch :: Regex Char -> String -> Either (RegexMatchFail Char) (String, String)
runMatch r s = runExcept (runStateT (match r) s)
