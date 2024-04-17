lex([], []).
lex('', '').
lex(Input, Token) :-
    Inputs = ['+', '-', '*', '/',
              '(', ')',
              'Int', 'Boolean', 'Void',
              'this', 'true', 'false',
              'println', 'call', 'new',
              'vardec', '=', 'while', 'break', 'if', 'return',
              'method', 'init', 'class', 'super'],
    Tokens = [op_token('+'), op_token('-'), op_token('*'), op_token('/'),
              lparen_token, rparen_token,
              type_token(int), type_token(boolean), type_token(void),
              exp_token(this), exp_token(true), exp_token(false),
              println_token, call_token, new_token,
              vardec_token, equals_token, while_token, break_token, if_token, return_token,
              method_token, init_token, class_token, super_token],
    nth0(Index, Inputs, Input),
    nth0(Index, Tokens, Token), !.
lex(X, integer_token(X)) :- integer(X), !.
lex(Input, Tokens) :- lex(Input, Tokens, []), !.
lex(X, string_token(X)). % Everything lexes, the parser will figure it out
lex([Input|[]], Tokens, Temp) :-
    lex(Input, Token),
    append(Temp, [Token], Temp2),
    Tokens = Temp2.
lex([Input|Rest], Tokens, Temp) :-
    lex(Input, Token),
    append(Temp, [Token], Temp2),
    lex(Rest, Tokens, Temp2).
