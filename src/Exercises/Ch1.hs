module Exercises.Ch1 where

import Control.Monad.State
import Data.Function (on)

type ID = String

data Binop = Plus | Minus | Times | Div deriving (Eq, Show)

data Stm
    = CompoundStm Stm Stm
    | AssignStm ID Expr
    | PrintStm [Expr]
    deriving (Eq, Show)

data Expr
    = IdExp ID
    | NumExp Int
    | OpExp Expr Binop Expr
    | EseqExp Stm Expr
    deriving (Eq, Show)

prog :: Stm
prog =
    CompoundStm
        (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
        (CompoundStm
            (AssignStm
                "b"
                (EseqExp
                    (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                    (OpExp (NumExp 10) Times (IdExp "a"))
                ))
            (PrintStm [IdExp "b"])
            )

-- | 1. Write a function maxargs that tells the maximum number of arguments
-- asdf asdf asdf  of any print statement within any subexpression of
-- a given statement.
--
-- >>> maxargs prog
-- 2
-- >>> maxargs (PrintStm [])
-- 0
-- >>> maxargs (PrintStm [EseqExp (PrintStm [IdExp "a", IdExp "b"]) (IdExp "a")])
-- 2
maxargs :: Stm -> Int
maxargs (CompoundStm a b) = (max `on` maxargs) a b
maxargs (AssignStm _ e) = maxArgsExpr e
maxargs (PrintStm exprs) = maximum (length exprs : map maxArgsExpr exprs)

-- | A helper function for maxargs. Finds the maximum number of arguments
-- in print statements embedded in expressions.
--
-- >>> maxArgsExpr (IdExp "a")
-- 0
-- >>> maxArgsExpr (EseqExp (PrintStm [IdExp "a"]) (IdExp "b"))
-- 1
maxArgsExpr :: Expr -> Int
maxArgsExpr (IdExp _) = 0
maxArgsExpr (OpExp a _ b) = (max `on` maxArgsExpr) a b
maxArgsExpr (EseqExp stm expr) = max (maxargs stm) (maxArgsExpr expr)
maxArgsExpr (NumExp _) = 0


-- | 2. Write a function that interprets a program in this language. To
-- write in a "functional" style -- without assignment or arrays --
-- maintain a list of (variable, integer) pairs, and produce new versions
-- of this list at each 'AssignStm'.
--
-- >>> interp (PrintStm [NumExp 2])
-- 2
interp :: Stm -> IO ()
interp stm = void $ runStateT (interpStm stm) []

type Table = [(ID, Int)]

interpStm :: Stm -> StateT Table IO Table
interpStm (AssignStm i expr) = do
    val <- interpExp expr
    modify ((i, val) :)
    get
interpStm (CompoundStm a b) =
    interpStm a >> interpStm b
interpStm (PrintStm es) = mapM_ f es >> get
    where
        f expr = interpExp expr >>= liftIO . print


interpExp :: Expr -> StateT Table IO Int
interpExp (IdExp i) = gets (iexpFromJust . lookup i)
    where
        iexpFromJust (Just a) = a
        iexpFromJust Nothing = error "ahh bitten by dangersss"
interpExp (NumExp i) = return i
interpExp (OpExp a op b) = perform op <$> interpExp a <*> interpExp b
interpExp (EseqExp stm expr) = interpStm stm >> interpExp expr

perform :: Binop -> Int -> Int -> Int
perform Plus = (+)
perform Minus = (-)
perform Div = div
perform Times = (*)

-- | 1.1: Given the following persistent binary search tree,

data Tree a
    = Leaf
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

empty :: Tree a
empty = Leaf

insert :: Ord a => a -> Tree a -> Tree a
insert a Leaf = Node Leaf a Leaf
insert a (Node l k r) =
    case a `compare` k of
         LT -> Node (insert a l) k r
         EQ -> Node l a r
         GT ->Node l k (insert a r)

-- | 1.1a: Implememt a `member` function that returns `true` if the item is
-- found, else false.
--
-- >>> member 'a' Leaf
-- False
-- >>> member 'a' Node (Node Leaf 'a' Leaf) 'b' Leaf
-- True
member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member a (Node l k r) =
    case a `compare` k of
         LT -> member a l
         EQ -> True
         GT -> member a r
