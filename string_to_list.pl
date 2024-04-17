string_to_list(String, List) :-
    string_to_list(String, List, []).

string_to_list("", List, List) :- !.

%If the whole string is one token
string_to_list(String, List, Temp) :-
    once(go_to_break(String, Index, _)),
    string_length(String, Index),
    string_to_atom(String, Atom),
    (   atom_number(Atom, Append) -> !
    ;   Append = Atom
    ),
    append(Temp, [Append], List).

%If the first character is a break character
string_to_list(String, List, Temp) :-
    once(go_to_break(String, Index, Char)),
    Index = 0,
    (   Char = 'lp' ->  append(Temp, ['('], New_Temp), sub_string(String, 1, _, 0, New_String)
    ;   Char = 'rp' ->  append(Temp, [')'], New_Temp), sub_string(String, 1, _, 0, New_String)
    ;   Char = 'ws' ->  New_Temp = Temp, sub_string(String, 1, _, 0, New_String)
    ;   Char = 'nl' ->  New_Temp = Temp, sub_string(String, 1, _, 0, New_String)
    ),
    string_to_list(New_String, List, New_Temp).

string_to_list(String, List, Temp) :-
    once(go_to_break(String, Index, Char)),
    \+Index = 0,
    (   Char = 'lp'
    ->  sub_string(String, 0, Index, _, Remove_String),
        string_to_atom(Remove_String, Atom),
        (   atom_number(Atom, Append) -> !
        ;   Append = Atom
        ),
        append(Temp, [Append, '('], New_Temp),
        Start_Index is Index + 1,
        sub_string(String, Start_Index, _, 0, New_String)
    ;   Char = 'rp'
    ->  sub_string(String, 0, Index, _, Remove_String),
        string_to_atom(Remove_String, Atom),
        (   atom_number(Atom, Append) -> !
        ;   Append = Atom
        ),
        append(Temp, [Append, ')'], New_Temp),
        Start_Index is Index + 1,
        sub_string(String, Start_Index, _, 0, New_String)
    ;   Char = 'ws'
    ->  sub_string(String, 0, Index, _, Remove_String),
        string_to_atom(Remove_String, Atom),
        (   atom_number(Atom, Append) -> !
        ;   Append = Atom
        ),
        append(Temp, [Append], New_Temp),
        Start_Index is Index + 1,
        sub_string(String, Start_Index, _, 0, New_String)
    ;   Char = 'nl'
    ->  sub_string(String, 0, Index, _, Remove_String),
        string_to_atom(Remove_String, Atom),
        (   atom_number(Atom, Append) -> !
        ;   Append = Atom
        ),
        append(Temp, [Append], New_Temp),
        Start_Index is Index + 1,
        sub_string(String, Start_Index, _, 0, New_String)
    ),
    string_to_list(New_String, List, New_Temp).

go_to_char(String, Index, Char) :-
    once(sub_string(String, Index, _, _, Char)).

% If the character isn't in the string, return an index outside the
% string
go_to_char(String, Index, Char) :-
    \+once(sub_string(String, _, _, _, Char)),
    string_length(String, Index).

go_to_break(String, Index, Char) :-
    go_to_char(String, LP_Index, '('),
    go_to_char(String, RP_Index, ')'),
    go_to_char(String, WS_Index, ' '),
    go_to_char(String, NL_Index, '\n'),
    once(min_list([LP_Index, RP_Index, WS_Index, NL_Index], Index)),
    once(nth0(Char_Index, [LP_Index, RP_Index, WS_Index, NL_Index], Index)),
    (   string_length(String, Index) -> Char = -1, !
    ;   Char_Index = 0 -> Char = 'lp'
    ;   Char_Index = 1 -> Char = 'rp'
    ;   Char_Index = 2 -> Char = 'ws'
    ;   Char_Index = 3 -> Char = 'nl'
    ).


