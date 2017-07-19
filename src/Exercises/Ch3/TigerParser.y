-- vim: ft=happy
{
{-# GHC_OPTIONS -fno-warn #-}
module Exercises.Ch3.TigerParser where

import Exercises.Ch2.TigerScanner (Token(..), TokenTy)
import qualified Exercises.Ch2.TigerScanner as Tok
import Data.ByteString.Lazy (ByteString)
import Data.String
}

%name parse
%tokentype { Token }

%token
    let      { T _ Tok.Let          }
    in       { T _ Tok.In           }
    while    { T _ Tok.While        }
    for      { T _ Tok.For          }
    to       { T _ Tok.To           }
    break    { T _ Tok.Break        }
    end      { T _ Tok.End          }
    function { T _ Tok.Function     }
    var      { T _ Tok.Var          }
    type     { T _ Tok.Type         }
    array    { T _ Tok.Array        }
    if       { T _ Tok.If           }
    then     { T _ Tok.Then         }
    else     { T _ Tok.Else         }
    do       { T _ Tok.Do           }
    of       { T _ Tok.Of           }
    nil      { T _ Tok.Nil          }
    '='      { T _ Tok.EqualSym     }
    '{'      { T _ Tok.OpenBracket  }
    '}'      { T _ Tok.CloseBracket }
    '('      { T _ Tok.OpenParen    }
    ')'      { T _ Tok.CloseParen   }
    ','      { T _ Tok.Comma        }
    ':'      { T _ Tok.Colon        }
    ':='     { T _ Tok.Assignment   }
    '.'      { T _ Tok.Period       }
    '['      { T _ Tok.OpenBrace    }
    ']'      { T _ Tok.CloseBrace   }
    ';'      { T _ Tok.Semicolon    }
    '-'      { T _ Tok.SubSym       }
    '+' { T _ Tok.PlusSym }
    '*' { T _ Tok.MulSym }
    '/' { T _ Tok.ForwardSlash }
    '<>' { T _ Tok.NotEqSym }
    '<'  { T _ Tok.LessThan }
    '>'  { T _ Tok.GreaterThan }
    '<=' { T _ Tok.AtMost }
    '>=' { T _ Tok.AtLeast }
    '&' { T _ Tok.Ampersand }
    '|' { T _ Tok.Pipe }
    NEG { T _ Tok.SubSym }

    int   { T _ (Tok.LitInt $$) }
    str   { T _ (Tok.LitStr $$) }
    ident { T _ (Tok.Ident $$)  }

%right in
%left '|'
%left '&'
%nonassoc '<' '>' '<>' '=' '<=' '>='
%left '+' '-'
%left '*' '/'
%left NEG

%%

Decs :: { [Decl] }
Decs : {- empty -} { []      }
     | Decs Dec    { $2 : $1 }

Dec :: { Decl }
Dec : TypeDec  { $1  }
    | VarDec   { VarDec $1 }
    | FunDec   { FunDec $1 }

TypeDec :: { Decl }
TypeDec : type ident '=' Type { TyDec (Ident $2) $4 }

Type :: { TypeP }
Type : ident          { TypeName (Ident $1) }
     | TyFields       { TypeFields $1                   }
     | array of ident { TypeArray (Ident $3)            }

TyFields :: { [TypeField] }
TyFields : {- empty -}               { []      }
         | '{' TyField TyFields1 '}' { $2 : $3 }

TyFields1 :: { [TypeField] }
TyFields1 : {- empty -}           { []      }
          | ',' TyField TyFields1 { $2 : $3 }

TyField :: { TypeField }
TyField : ident ':' ident { TypeField (Ident $1) (Ident $3) }

VarDec :: { VarDecl }
VarDec : var ident OptAnn ':=' Expr { mkVarDecl (Ident $2) $3 $5 }

FunDec :: { FunDecl }
FunDec : function ident '(' TyFields ')' OptAnn '=' Expr { case $6 of
       Just ty -> Func (Ident $2) $4 ty $8 
       Nothing -> Proc (Ident $2) $4 $8 }

OptAnn :: { Maybe Ident }
OptAnn : {- empty -} { Nothing             }
       | ':' ident   { Just (Ident $2) }

LValue :: { LValue }
LValue : ident { LIdent (Ident $1) }
       | LValue '.' ident { LDot $1 (Ident $3) }
       | LValue '[' Expr ']' { LSubscript $1 $3 }

Exprs :: { [Expr] }
Exprs : {- empty -}     { []      }
      |  Expr ';' Exprs { $1 : $3 }

Expr :: { Expr }
Expr : let Decs in Expr { ELet $2 $4 }
     | nil { ENil }
     | '(' Exprs ')' { ESeq $2 }
     | int { EInt $1 }
     | str { EStr $1 }
     | ident BraceExpr { $2 $1 }
     | NEG Expr  { BinOp (EInt 0) (ArithOp OMinus) $2 }
     | Expr BinOp Expr { case $2 of
        Left b ->
            case b of
                OOr -> EIfElse $1 (EInt 1) $3
                OAnd -> EIfElse $1 $3 (EInt 0)
        Right a -> 
            BinOp $1 a $3 
        }
     | LValue ':=' Expr { EAssign $1 $3 }
     | if Expr then Expr MElse { case $5 of
            Nothing -> EIf $2 $4 
            Just e  -> EIfElse $2 $4 e }
     | while Expr do Expr { EWhile $2 $4 }
     | for ident ':=' Expr to Expr do Expr { EFor EscYes (Ident $2) $4 $6 $8 }
     | break { EBreak }

BraceExpr :: { ByteString -> Expr }
BraceExpr : '(' FunArgs ')' { \i -> FunCall (Ident i) $2 }
           | '[' RecAssns ']' { \i -> ERecCreate (Ident i) $2 }
           | '[' Expr ']' of Expr { \i -> EArrCreate (Ident i) $2 $5 }

MElse :: { Maybe Expr }
MElse : { Nothing }
      | else Expr { Just $2 }
    

RecAssns :: { [RecAssn] }
RecAssns :                   { [] }
         | RecAssn RecAssns1 { $1 : $2 }

RecAssn :: { RecAssn }
RecAssn : ident '=' Expr { RecAssn (Ident $1) $3 }

RecAssns1 :: { [RecAssn] }
RecAssns1 : ',' RecAssn RecAssns1 { $2 : $3 }
          |                       { [] }

FunArgs :: { [Expr] }
FunArgs : { [] }
        | Expr FunArgs1 { $1 : $2 }

FunArgs1 :: { [Expr] }
FunArgs1 :  { [] }
         | ',' Expr FunArgs1 { $2 : $3 }

BinOp :: { Either BoolOp Op }
BinOp : '+' { Right $ ArithOp OPlus }
      | '-' { Right $ ArithOp OMinus }
      | '*' { Right $ ArithOp OTimes }
      | '/' { Right $ ArithOp ODivide }
      | '=' { Right $ CompOp OEQ }
      | '<>' { Right $ CompOp ONEQ }
      | '<'  { Right $ CompOp OLT }
      | '>'  { Right $ CompOp OGT }
      | '<=' { Right $ CompOp OLTE }
      | '>=' { Right $ CompOp OGTE }
      | '&' { Left OAnd }
      | '|' { Left OOr }

{

data Expr 
    = ELet [Decl] Expr
    | EVar VarDecl
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

data RecAssn = RecAssn Ident Expr

data Op = CompOp CompOp | ArithOp ArithOp

data BoolOp = OAnd | OOr

data CompOp = OEQ | OGT | OGTE | OLT | OLTE | ONEQ

data ArithOp = OPlus | OMinus | OTimes | ODivide

happyError _ = error "parse error"

newtype Ident = Ident { unIdent :: ByteString }
    deriving (Eq, Ord, Show)

data Escape = EscYes | EscNo

data Decl
    = TyDec Ident TypeP
    | VarDec VarDecl
    | FunDec FunDecl

data LValue 
    = LIdent Ident
    | LDot LValue Ident
    | LSubscript LValue Expr

data FunDecl 
    = Func Ident [TypeField] Ident Expr
    | Proc Ident [TypeField] Expr

mkVarDecl :: Ident -> Maybe Ident -> Expr -> VarDecl
mkVarDecl = VarDecl EscYes

data VarDecl
    = VarDecl Escape Ident (Maybe Ident) Expr

data TypeDecl = TypeDecl Ident TypeP

data TypeP
    = TypeName Ident
    | TypeFields [TypeField]
    | TypeArray Ident

data TypeField
    = TypeField Ident Ident

instance IsString Ident where
    fromString = Ident . fromString

}
