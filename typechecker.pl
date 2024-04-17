%Checks if an expression is a number or computes to a number
num_exp(integer_exp(_)).
num_exp(op_exp(op_token(Op), Exp1, Exp2)) :-
    member(Op, ['+', '-', '*', '/']),
    num_exp(Exp1),
    num_exp(Exp2).

%Checks if an expression is a boolean or computes to a boolean
bool_exp(true_token).
bool_exp(false_token).
bool_exp(op_exp(op_token(Op), Exp1, Exp2)) :-
    member(Op, ['<', '>', '==']),
    num_exp(Exp1),
    num_exp(Exp2).
