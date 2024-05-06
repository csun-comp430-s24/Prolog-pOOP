generate_one(integer_token(X), [X]).
generate_one(this_token, ['this']). %Might need to be changed
generate_one(true_token, ['true']).
generate_one(false_token, ['false']).
generate_one(string_token(String), ['"', String, '"']).

%Converts (Op Exp1 Exp2) to (Exp1 Op Exp2)
generate_one(op_exp(op_token(Op), Exp1, Exp2), C_Code) :-
    generate_one(Exp1, Exp1_Code),
    generate_one(Exp2, Exp2_Code),
    append(['('|Exp1_Code], [Op|Exp2_Code], C_Code_2),
    append(C_Code_2, [')'], C_Code).

%Converts (println Exp) to printf("Exp");
generate_one(println_exp(string_exp(String)), ['println("', String, '");']).

%Converts (println Exp) to printf("%i", Exp); Need to figure out Void
generate_one(println_exp(Exp), C_Code) :-
    generate_one(Exp, Exp_Code),
    append(['println("%i", '|Exp_Code], [');'], C_Code).

%Converts (call exp methodname exp*) to

%Converts (new classname exp*) to (struct classname){exp,*}
generate_one(new_exp(Classname, Exps), C_Code) :-
    generate_comma_list(Exps, C_Exps),
    append(['(struct', Classname, '){'|C_Exps], ['}'], C_Code).

generate_list(List, C_Code) :-
    generate_list(List, C_Code, []).

generate_list([], C_Code, C_Code).
generate_list([Head|[]], C_Code, Temp) :-
    generate_one(Head, One_C_Code),
    append(Temp, One_C_Code, New_Temp),
    generate_list([], C_Code, New_Temp).

generate_comma_list(List, C_Code) :-
    generate_comma_list(List, C_Code, []).

generate_comma_list([], C_Code, C_Code).
generate_comma_list([Head|[]], C_Code, Temp) :- %The last one in the list doesn't get a comma after it
    generate_one(Head, One_C_Code),
    append(Temp, One_C_Code, New_Temp),
    generate_comma_list([], C_Code, New_Temp).
generate_comma_list([Head|Tail], C_Code, Temp) :-
    generate_one(Head, One_C_Code),
    append(One_C_Code, [','], One_Comma),
    append(Temp, One_Comma, New_Temp),
    generate_comma_list(Tail, C_Code, New_Temp).

%Converts (vardec type var) for non-objects to type var;
generate_one(vardec(Type, Varname), [C_Type, Varname, ';']) :-
    (   Type = 'Int' -> C_Type = 'int'
    ;   Type = 'Boolean' -> C_Type = 'bool'
    ;   Type = 'Void' -> C_Type = '' %Figure this out
    ).

%Converts (vardec type var) for objects to struct type var;
generate_one(vardec(Type, Varname), ['struct', Type, Varname, ';']).

%Converts (= var exp) to var = exp;
generate_one(asmt_stmt(Varname, Exp), [Varname, '='|C_Code]) :-
    generate_one(Exp, C_Exp),
    append(C_Exp, [';'], C_Code).

%Converts (while exp stmt*) to while(exp){stmt*}
generate_one(while_stmt(Exp, Stmts), ['while('|C_Code]) :-
    generate_one(Exp, C_Exp),
    append(C_Exp, ['{'], C_Code_1),
    generate_list(Stmts, C_Stmts),
    append(C_Code_1, C_Stmts, C_Code_2),
    append(C_Code_2, ['}'], C_Code).

%break
generate_one(break_stmt, ['break;']).

%Converts (if exp stmt) to if(exp){stmt} Blocks

%return
generate_one(return_stmt, 'return;').

generate_one(return_exp_stmt(Exp), ['return '|C_Code]) :-
    generate_one(Exp, C_Exp),
    append(C_Exp, [';'], C_Code).
