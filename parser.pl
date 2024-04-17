% exp ::= VARIABLE | STRING | INTEGER |
%        `this` |
%        `true` | 'false' |
%        `(` `println` exp `)` |
%        `(` op exp exp `)` |
%        `(` `call` exp methodname exp* `)` |
%        `(` `new` classname exp* `)`

% exp: InputTokens, Exp, OutputTokens

%exp ::= INTEGER | 'this' | 'true' | 'false'
exp([Token|Rest],
    Exp,
    Rest) :-
    (   Token = integer_token(X) -> Exp = integer_exp(X)
    ;   Token = this_token -> Exp = this_exp
    ;   Token = true_token -> Exp = true_exp
    ;   Token = false_token -> Exp = false_exp
    ).

%exp ::= `(` op exp exp `)`
exp([lparen_token, op_token(Op)|Rest1],
    op_exp(op_token(Op), Exp1, Exp2),
    Rest3) :-
    exp(Rest1, Exp1, Rest2),
    exp(Rest2, Exp2, [rparen_token|Rest3]).

%exp ::= `(` `println` exp `)`
exp([lparen_token, println_token|Rest1],
    println_exp(Exp),
    Rest2) :-
    exp(Rest1, Exp, [rparen_token|Rest2]).

%exp ::= `(` `call` exp methodname exp* `)`
exp([lparen_token, call_token|Rest1],
    call_exp(Exp1, Methodname, Exps2),
    Rest3) :-
    exp(Rest1, Exp1, [string_token(Methodname)|Rest2]),
    once(parse_list(exp, Rest2, Exps2, [rparen_token|Rest3])).

%exp ::= `(` `new` classname exp* `)`
exp([lparen_token, new_token, string_token(Classname)|Rest1],
    new_exp(Classname, Exps),
    Rest2) :-
    once(parse_list(exp, Rest1, Exps, [rparen_token|Rest2])).

%exp ::= STRING
exp([string_token(X)|Rest], string_exp(X), Rest).

%vardec ::= `(` `vardec` type var `)`
vardec([lparen_token, vardec_token, type_token(Type), string_token(Varname), rparen_token|Rest],
    vardec(Type, Varname),
    Rest).

vardec([lparen_token, vardec_token, string_token(Type), string_token(Varname), rparen_token|Rest],
    object_vardec(Type, Varname),
    Rest).

%stmt ::= vardec
stmt(X, Y, Z) :- vardec(X, Y, Z).

%stmt ::= `(` `=` var exp `)`
stmt([lparen_token, equals_token, string_token(Varname)|Rest1],
    asmt_stmt(Varname, Exp),
    Rest2) :-
    exp(Rest1, Exp, [rparen_token|Rest2]).

%stmt ::= `(` `while` exp stmt* `)`
stmt([lparen_token, while_token|Rest1],
    while_stmt(Exp, Stmts),
    Rest3) :-
    exp(Rest1, Exp, Rest2),
    once(parse_list(stmt, Rest2, Stmts, [rparen_token|Rest3])).

%stmt ::= 'break'
stmt([break_token|Rest], break_stmt, Rest).

%stmt ::= `(` `if` exp stmt [stmt] `)`
stmt([lparen_token, if_token|Rest1],
    if_stmt(Exp, Stmt),
    Rest3) :-
    exp(Rest1, Exp, Rest2),
    stmt(Rest2, Stmt, [rparen_token|Rest3]).

stmt([lparen_token, if_token|Rest1],
    ifelse_stmt(Exp, Stmt1, Stmt2),
    Rest4) :-
    exp(Rest1, Exp, Rest2),
    stmt(Rest2, Stmt1, Rest3),
    stmt(Rest3, Stmt2, [rparen_token|Rest4]).

%stmt ::= `(` return [exp] `)`
stmt([lparen_token, return_token, rparen_token|Rest], return_stmt, Rest).
stmt([lparen_token, return_token|Rest1],
     return_exp_stmt(Exp),
     Rest2) :-
    exp(Rest1, Exp, [rparen_token|Rest2]).

%methoddef ::= `(` `method` methodname
%                  `(` vardec* `)` type stmt* `)`
methoddef([lparen_token, method_token, string_token(Methodname), lparen_token|Rest1],
           method_def(Methodname, Vardecs, Type, Stmts),
           Rest3) :-
    once(parse_list(vardec, Rest1, Vardecs, [rparen_token, type_token(Type)|Rest2])),
    once(parse_list(stmt, Rest2, Stmts, [rparen_token|Rest3])).

%constructor ::= `(` `init` `(` vardec* `)`
%                    [`(` `super` exp* `)`]
%                    stmt* `)`
constructor([lparen_token, init_token, lparen_token|Rest1],
            constructor_parent(Vardecs, Stmts),
            Rest3) :-
    once(parse_list(vardec, Rest1, Vardecs, [rparen_token|Rest2])),
    once(parse_list(stmt, Rest2, Stmts, [rparen_token|Rest3])).

constructor([lparen_token, init_token, lparen_token|Rest1],
            constructor_child(Vardecs, Exps, Stmts),
            Rest4) :-
    once(parse_list(vardec, Rest1, Vardecs, [rparen_token, lparen_token, super_token|Rest2])),
    once(parse_list(exp, Rest2, Exps, [rparen_token|Rest3])),
    once(parse_list(stmt, Rest3, Stmts, [rparen_token|Rest4])).

%classdef ::= `(` `class` classname [classname]
%               `(` vardec* `)`
%               constructor
%               methoddef* `)`
classdef([lparen_token, class_token, string_token(Classname), lparen_token|Rest1],
         classdef(Classname, Vardecs, Constructor, Methoddefs),
         Rest4) :-
    once(parse_list(vardec, Rest1, Vardecs, [rparen_token|Rest2])),
    constructor(Rest2, Constructor, Rest3),
    once(parse_list(methoddef, Rest3, Methoddefs, [rparen_token|Rest4])).

classdef([lparen_token, class_token, string_token(Classname1), string_token(Classname2), lparen_token|Rest1],
         classdef(Classname1, Classname2, Vardecs, Constructor, Methoddefs),
         Rest4) :-
    once(parse_list(vardec, Rest1, Vardecs, [rparen_token|Rest2])),
    constructor(Rest2, Constructor, Rest3),
    once(parse_list(methoddef, Rest3, Methoddefs, [rparen_token|Rest4])).

%program ::= classdef* stmt+
program(Input, program(Classdefs, Stmts), Leftovers) :-
    once(parse_list(classdef, Input, Classdefs, Rest1)),
    once(parse_list(stmt, Rest1, Stmts, Leftovers)),
    length(Stmts, Length),
    Length > 0.

parse(A, B, C, D) :-
    (   A = 'exp' -> exp(B, C, D)
    ;   A = 'vardec' -> vardec(B, C, D)
    ;   A = 'stmt' -> stmt(B, C, D)
    ;   A = 'methoddef' -> methoddef(B, C, D)
    ;   A = 'constructor' -> constructor(B, C, D)
    ;   A = 'classdef' -> classdef(B, C, D)
    ).

parse_list(Parse_Type, Tokens1, [Classdef|RestClassdefs], TokensFinal) :-
    parse(Parse_Type, Tokens1, Classdef, Tokens2),
    parse_list(Parse_Type, Tokens2, RestClassdefs, TokensFinal).
parse_list(_, Tokens, [], Tokens).


