-- vim: ft=happy
{
module Exercises.Ch3.TigerParser where

import Exercises.Ch2.TigerScanner (Token(..), TokenTy)
import qualified Exercises.Ch2.TigerScanner as Tok
import Data.ByteString.Lazy (ByteString)
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
Dec : TypeDec  { TyDec $1  }
    | VarDec   { VarDec $1 }
    | FunDec   { FunDec $1 }

TypeDec :: { TypeDecl }
TypeDec : type ident '=' Type { TypeDecl (typeIdent $2) $4 }

Type :: { Type }
Type : ident          { TypeName (typeIdent $1) }
     | TyFields       { TypeFields $1                   }
     | array of ident { TypeArray (Ident $3)            }

TyFields :: { [TypeField] }
TyFields : {- empty -}               { []      }
         | '{' TyField TyFields1 '}' { $2 : $3 }

TyFields1 :: { [TypeField] }
TyFields1 : {- empty -}           { []      }
          | ',' TyField TyFields1 { $2 : $3 }

TyField :: { TypeField }
TyField : ident ':' ident { TypeField (Ident $1) (typeIdent $3) }

VarDec :: { VarDecl }
VarDec : var ident OptAnn ':=' Expr { VarDecl (Ident $2) $3 $5 }

FunDec :: { FunDecl }
FunDec : function ident '(' TyFields ')' OptAnn '=' Expr { case $6 of
       Just ty -> Func (Ident $2) $4 ty $8 
       Nothing -> Proc (Ident $2) $4 $8 }

OptAnn :: { Maybe TypeIdent }
OptAnn : {- empty -} { Nothing             }
       | ':' ident   { Just (typeIdent $2) }

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
     | NEG Expr  { ENeg $2 }
     | Expr BinOp Expr { BinOp $1 $2 $3 }
     | ident '{' RecAssns '}' { ERecCreate (typeIdent $1) $3 }

RecAssns :: { [RecAssn] }
RecAssns :                   { [] }
         | RecAssn RecAssns1 { $1 : $2 }

RecAssn :: { RecAssn }
RecAssn : ident '=' Expr { RecAssn (Ident $1) $3 }

RecAssns1 :: { [RecAssn] }
RecAssns1 : ',' RecAssn RecAssns1 { $2 : $3 }
          |                       { [] }

BinOp :: { Op }
BinOp : '+' { ArithOp OPlus }
      | '-' { ArithOp OMinus }
      | '*' { ArithOp OTimes }
      | '/' { ArithOp ODivide }
      | '=' { CompOp OEQ }
      | '<>' { CompOp ONEQ }
      | '<'  { CompOp OLT }
      | '>'  { CompOp OGT }
      | '<=' { CompOp OLTE }
      | '>=' { CompOp OGTE }
      | '&' { BoolOp OAnd }
      | '|' { BoolOp OOr }

-- up next: array creation
      
{

data Expr 
    = ELet [Decl] Expr
    | ENil
    | ESeq [Expr]
    | EInt Int
    | EStr ByteString
    | ENeg Expr
    | BinOp Expr Op Expr
    | ERecCreate TypeIdent [RecAssn]

data RecAssn = RecAssn Ident Expr

data Op = CompOp CompOp | ArithOp ArithOp | BoolOp BoolOp

data BoolOp = OAnd | OOr

data CompOp = OEQ | OGT | OGTE | OLT | OLTE | ONEQ

data ArithOp = OPlus | OMinus | OTimes | ODivide

happyError _ = error "parse error"

typeIdent :: ByteString -> TypeIdent
typeIdent = TypeIdent . Ident

newtype Ident = Ident { unIdent :: ByteString }
    deriving (Eq, Show)

newtype TypeIdent = TypeIdent { unTypeIdent :: Ident }
    deriving (Eq, Show)

data Decl
    = TyDec TypeDecl
    | VarDec VarDecl
    | FunDec FunDecl

data LValue 
    = LIdent Ident
    | LDot LValue Ident
    | LSubscript LValue Expr

data FunDecl 
    = Func Ident [TypeField] TypeIdent Expr
    | Proc Ident [TypeField] Expr

data VarDecl
    = VarDecl Ident (Maybe TypeIdent) Expr

data TypeDecl = TypeDecl TypeIdent Type

data Type
    = TypeName TypeIdent
    | TypeFields [TypeField]
    | TypeArray Ident

data TypeField
    = TypeField Ident TypeIdent

}
