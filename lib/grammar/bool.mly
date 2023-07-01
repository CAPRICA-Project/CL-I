%token X__ERR_TOK "<???>"
%token X__ERR_TOK2 "<????>"
%%

%public %inline True(X, Y): X { $1 }
%public %inline False(X, Y): Y { $1 }

%public %inline ifte(B, THEN, ELSE): B(THEN, ELSE) { $1 }
%public %inline ift(B, THEN): B(THEN, epsilon) { $1 }

%public %inline ifnot(B, THEN, ELSE): ifte(B, ELSE, THEN) { $1 }
%public %inline ifnot_(B, THEN): ifnot(B, THEN, epsilon) { $1 }

%public %inline unless(B, THEN): ifnot_(B, THEN) { $1 }

%public ifand(B1, B2, THEN, ELSE): ifte(B1, ifte(B2, THEN, ELSE), ELSE) { $1 }
%public ifand_(B1, B2, THEN): ifand(B1, B2, THEN, epsilon) { $1 }

%public ifor(B1, B2, THEN, ELSE): ifte(B1, THEN, ifte(B2, THEN, ELSE)) { $1 }
%public ifor_(B1, B2, THEN): ifor(B1, B2, THEN, epsilon) { $1 }

%public ifnand(B1, B2, THEN, ELSE): ifte(B1, ifte(B2, ELSE, THEN), THEN) { $1 }
%public ifnand_(B1, B2, THEN): ifnand(B1, B2, THEN, epsilon) { $1 }

%public ifnor(B1, B2, THEN, ELSE): ifte(B1, ELSE, ifte(B2, ELSE, THEN)) { $1 }
%public ifnor_(B1, B2, THEN): ifnor(B1, B2, THEN, epsilon) { $1 }

%public ifxor(B1, B2, THEN, ELSE): ifte(B1, ifte(B2, ELSE, THEN), ifte(B2, THEN, ELSE)) { $1 }
%public ifxor_(B1, B2, THEN): ifxor(B1, B2, THEN, epsilon) { $1 }

%public ifxnor(B1, B2, THEN, ELSE): ifte(B1, ifte(B2, THEN, ELSE), ifte(B2, ELSE, THEN)) { $1 }
%public ifxnor_(B1, B2, THEN): ifxnor(B1, B2, THEN, epsilon) { $1 }

%public %inline epsilon: {}

%public %inline require(B): ifte(B, epsilon, X__ERR_TOK) { $1 }
%public %inline also_require(B): ifte(B, epsilon, X__ERR_TOK2) { $1 }
%public %inline refuse_if(B): ifte(B, X__ERR_TOK, epsilon) { $1 }
%public %inline also_refuse_if(B): ifte(B, X__ERR_TOK2, epsilon) { $1 }
%public %inline reject_if(B): refuse_if(B) { $1 }
%public %inline also_reject_if(B): also_refuse_if(B) { $1 }
