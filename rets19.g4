/*
 MIT License https://github.com/darnjo/rets19/blob/master/LICENSE
 Copyright (c) 2018 Joshua Darnell (josh@kurotek.com)
*/

grammar rets19;

exp
  : orExp
  | collection
  ;

orExp
  : andExp (OR andExp)*
  ;

andExp
  : notExp (AND notExp)*
  ;

notExp
  : NOT notExp
  | eqExp
  ;

eqExp
  : cmpExp
  | cmpExp EQ cmpExp
  | cmpExp NE cmpExp
  ;

cmpExp
  : cntExp
  | cntExp LTE cntExp
  | cntExp GTE cntExp
  | cntExp LT cntExp
  | cntExp GT cntExp
  ;

cntExp
  : sumExp
  | sumExp CONTAINS sumExp
  | sumExp IN list
  ;

sumExp
  : prodExp ((PLUS|MINUS|CONCAT) prodExp)*
  ;

prodExp
  : atomExp ((ASTERISK|SLASH|MOD) atomExp)*
  ;

atomExp
  : LPAREN exp RPAREN
  | list
  | funcExp
  | value
  ;

list
  : LPAREN (exp (COMMA exp)*)? RPAREN
  ;

funcExp
  : func LPAREN (param (COMMA param)*)? RPAREN
  ;

collection
  : (LIST | SET) LPAREN (exp (COMMA exp)*)? RPAREN
  | (UNION | INTERSECTION | DIFFERENCE) LPAREN (collection COMMA collection (COMMA collection)*)? RPAREN
  ;

func
  : SPECFUNC
  | ALPHANUM
  ;

param
    : exp
    ;

value
  : fieldName
  | specValue
  | charValue
  | intValue
  | floatValue
  | timeValue
  ;

fieldName
  : (LAST)? RETSNAME
  | LBRACKET (LAST)? RETSNAME RBRACKET
  ;

specValue : DOT RETSNAME DOT;

charValue : QUOTED_TERM;

timeValue : HASH RETSDATETIME HASH;

intValue : (PLUS|MINUS)? DIGIT+ ;

floatValue : intValue DOT DIGIT+;


// Tokens - may be moved to lexer file
CONCAT : PIPE;
LPAREN : '(' ;
RPAREN : ')' ;
SQUOTE : '\'' ;
QUOTE : '"' ;
DOT : '.' ;
ASTERISK : '*';
SLASH : '/';
EXCLAMATION : '!';

OR  : '.OR.';
AND : '.AND.';
NOT : '.NOT.';

EQ  : '=';
NE  : EXCLAMATION EQ;
LT  : '<';
LTE : LT EQ;
GT  : '>';
GTE : GT EQ;

CONTAINS : '.CONTAINS.';
IN : '.IN.';
COMMA: ',';
PLUS: '+';
MINUS: '-';
MOD: '.MOD.';
PIPE: '|';
LBRACKET: '[';
RBRACKET: ']';
HASH: '#';
IIF: 'IIF';
LAST: 'LAST';
LIST: 'LIST';
SET: 'SET';
DIFFERENCE: 'DIFFERENCE';
INTERSECTION: 'INTERSECTION';
UNION: 'UNION';
TRUE: 'TRUE';
FALSE: 'FALSE';
EMPTY: 'EMPTY';
TODAY: 'TODAY';
NOW: 'NOW';
ENTRY: 'ENTRY';
OLDVALUE: 'OLDVALUE';
USERID: 'USERID';
USERCLASS: 'USERCLASS';
USERLEVEL: 'USERLEVEL';
AGENTCODE: 'AGENTCODE';
BROKERCODE: 'BROKERCODE';
BROKERBRANCH: 'BROKERBRANCH';
UPDATEACTION: 'UPDATEACTION';
ANY: 'any';

//special tokens
RETSNAME
  : DICTNAME
  | SPECOP
  ;

//TODO: dynamically fill in your dictnames here
DICTNAME
  : 'ListPrice'
  | 'Status'
  | 'CloseDate'
  | 'Bedrooms'
  | 'Bathrooms'
  ;

SPECFUNC
  : IIF
  ;

SPECOP
  : EMPTY
  | TRUE
  | FALSE
  | TODAY
  | NOW
  | ENTRY
  | OLDVALUE
  | USERID
  | USERCLASS
  | USERLEVEL
  | AGENTCODE
  | BROKERCODE
  | BROKERBRANCH
  | UPDATEACTION
  | ANY
  ;

RETSDATETIME: '##TODO##';
ALPHA: ('a'..'z' | 'A'..'Z');
DIGIT: ('0'..'9');

ALPHANUM: ALPHA (ALPHA|DIGIT)*;

QUOTED_TERM
    :   QUOTE (~[\\"])*? QUOTE
    |   SQUOTE (~[\\'])*? SQUOTE
    ;

//added support for c++ style comments
SLASH_STAR_COMMENT  : '/*' .+? '*/' -> skip ;
SLASH_SLASH_COMMENT : '//' .+? ('\n'|EOF) -> skip ;

WS : [ \t\n\r]+ -> skip ;
