grammar Grammar;
program: relations '?-' body? '.';
relations: (relation)*;
relation: head ':-' body '.' | head '.';
head: atom;
body: (atom ',')* atom;
atom: ident | ident '(' args ')';
args: ((var | atom) ',')* (var | atom);
ident: IDENT;
var: VAR;

VAR: [A-Z] ([a-z] | [0-9] | [A-Z])*;
IDENT: [a-z] ([a-z] | [0-9] | [A-Z])*;
WS : [ \t\r\n]+ -> skip;