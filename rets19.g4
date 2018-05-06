/*
 MIT License https://github.com/darnjo/rets19/blob/master/LICENSE
 Copyright (c) 2018 Joshua Darnell (josh@kurotek.com)
*/

grammar rets19;

exp
    : orExp
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

funcExp
    : func LPAREN (param (COMMA param)*)? RPAREN
    ;

func
    : ALPHANUM
    ;

list
    : LPAREN (exp (COMMA exp)*)? RPAREN
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
LAST: 'LAST';

//special tokens
RETSNAME
    : DICTNAME
    | SPECOP
    ;

//TODO: fill in dictnames
DICTNAME
    : 'ListPrice'
    | 'Status'
    | 'CloseDate'
    | 'Bedrooms'
    | 'Bathrooms'
    ;

SPECOP
    :   'EMPTY'
    |   'TRUE'
    |   'FALSE'
    |   'TODAY'
    |   'NOW'
    |   'ENTRY'
    |   'OLDVALUE'
    |   'USERID'
    |   'USERCLASS'
    |   'USERLEVEL'
    |   'AGENTCODE'
    |   'BROKERCODE'
    |   'BROKERBRANCH'
    |   'UPDATEACTION'
    |   'any'
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
