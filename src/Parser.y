{
module Parser where

import Lexer
import Expr
import Value

}

%name expr
%tokentype { Token }
%error { parseError }

%token
    let   { TokenLet }
    in    { TokenIn }
    NUM   { TokenNum $$ }
    VAR   { TokenSym $$ }
    '\\'  { TokenLambda }
    '->'  { TokenArrow }
    '='   { TokenEq }
    '+'   { TokenAdd }
    '-'   { TokenSub }
    '*'   { TokenMul }
    '('   { TokenLParen }
    ')'   { TokenRParen }

%left '+' '-'
%left '*'
%%

Expr : '\\' VAR '->' Expr          { ELam $2 $4 }
     | Form                        { $1 }

Form : Form '+' Form               { EPrim Add $1 $3 }
     | Form '-' Form               { EPrim Mul $1 $3 }
     | Juxt                        { $1 }

Juxt : Juxt Atom                   { EApp $1 $2 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | NUM                         { EInt $1 }
     | VAR                         { EVar $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
}