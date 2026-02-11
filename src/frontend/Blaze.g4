grammar Blaze;

FN: 'fn';
VAR: 'var';
CONST: 'const';
RETURN: 'return';
IF: 'if';
ELSE: 'else';
WHILE: 'while';
PRE: 'pre';
POST: 'post';
PLUS: '+';
MINUS: '-';
STAR: '*';
SLASH: '/';
NOT: '!';
LT: '<';
LE: '<=';
GT: '>';
GE: '>=';
EQ_EQ: '==';
NEQ: '!=';
TRUE: 'true';
FALSE: 'false';

program
  : functionDecl* EOF
  ;

// Will later be extended qualified ids
typeName
  : Identifier
  ;

block
  : '{' stmt* '}'
  ;

stmt
  : declStmt
  | returnStmt
  | ifStmt
  | whileStmt
  | exprStmt
  | assignmentStmt
  | block
  ;

declStmt
  : (VAR | CONST) Identifier ':' typeName '=' expr ';'
  ;

returnStmt
  : RETURN expr? ';'
  ;

ifStmt
  : IF '(' expr ')' block (ELSE (block | ifStmt))?
  ;

whileStmt
  : WHILE '(' expr ')' block
  ;

assignmentStmt
  : Identifier '=' expr ';'
  ;

exprStmt
  : expr ';'
  ;

expr
  : compExpr
  ;

compExpr
  : addExpr ((LT | LE | GT | GE | EQ_EQ | NEQ) addExpr)?
  ;

addExpr
  : mulExpr ((PLUS | MINUS) mulExpr)*
  ;

mulExpr
  : unaryExpr ((STAR | SLASH) unaryExpr)*
  ;

unaryExpr
  : NOT unaryExpr
  | primaryExpr
  ;

primaryExpr
  : Identifier '(' argList? ')'  # CallExpr
  | Identifier                  # VarExpr
  | Integer                     # IntExpr
  | TRUE                        # BoolExpr
  | FALSE                       # BoolExpr
  | '(' expr ')'                # ParenExpr
  ;

argList
  : expr (',' expr)*
  ;

functionDecl
  : FN Identifier '(' paramList? ')' ('->' typeName)?
    fnSpec*
    block
  ;

paramList
  : param (',' param)*
  ;

param
  : Identifier ':' typeName
  ;

fnSpec
  : PRE '{' exprStmt* '}'
  | POST '{' ( Identifier ':')? exprStmt* '}'
  ;

Integer
  : [0-9]+
  ;

Identifier
  : [a-zA-Z_][a-zA-Z_0-9]*
  ;

LineComment
  : '//' ~[\r\n]* -> skip
  ;

BlockComment
  : '/*' .*? '*/' -> skip
  ;

Whitespace
  : [ \t\r\n]+ -> skip
  ;
