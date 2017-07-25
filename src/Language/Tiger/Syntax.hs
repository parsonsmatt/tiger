{-# LANGUAGE DeriveLift #-}

module Language.Tiger.Syntax where

import           Data.ByteString.Lazy       (ByteString)
import           Data.String
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax

data Expr
    = ELet [Decl] Expr
    | EVar VarDecl
    | ELValue LValue
    | ENil
    | ESeq [Expr]
    | EInt Int
    | EStr ByteString
    | BinOp Expr Op Expr
    | ERecCreate Ident [RecAssn]
    | FunCall Ident [Expr]
    | EArrCreate Ident Expr Expr
    | EAssign LValue Expr
    | EIfElse Expr Expr Expr
    | EIf Expr Expr
    | EWhile Expr Expr
    | EFor Escape Ident Expr Expr Expr
    | EBreak
    deriving (Eq, Show, Lift)

data RecAssn
    = RecAssn
    { recAssnIdent :: Ident
    , recAssnExpr  :: Expr
    } deriving (Eq, Show, Lift)

data Op = CompOp CompOp | ArithOp ArithOp
    deriving (Eq, Show, Lift)

data BoolOp = OAnd | OOr
    deriving (Eq, Show, Lift)

data CompOp = OEQ | OGT | OGTE | OLT | OLTE | ONEQ
    deriving (Eq, Show, Lift)

data ArithOp = OPlus | OMinus | OTimes | ODivide
    deriving (Eq, Show, Lift)

newtype Ident = Ident { unIdent :: ByteString }
    deriving (Eq, Ord, Show, Lift)

data Escape = EscYes | EscNo
    deriving (Eq, Show, Lift)

data Decl
    = TyDec Ident TypeP
    | VarDec VarDecl
    | FunDec FunDecl
    deriving (Eq, Show, Lift)

data LValue
    = LIdent Ident
    | LDot LValue Ident
    | LSubscript LValue Expr
    deriving (Eq, Show, Lift)

data FunDecl
    = Func Ident [TypeField] Ident Expr
    | Proc Ident [TypeField] Expr
    deriving (Eq, Show, Lift)

mkVarDecl :: Ident -> Maybe Ident -> Expr -> VarDecl
mkVarDecl = VarDecl EscYes

data VarDecl
    = VarDecl Escape Ident (Maybe Ident) Expr
    deriving (Eq, Show, Lift)

data TypeDecl = TypeDecl Ident TypeP
    deriving (Eq, Show, Lift)

data TypeP
    = TypeName Ident
    | TypeFields [TypeField]
    | TypeArray Ident
    deriving (Eq, Show, Lift)

data TypeField
    = TypeField Ident Ident
    deriving (Eq, Show, Lift)

instance IsString Ident where
    fromString = Ident . fromString
