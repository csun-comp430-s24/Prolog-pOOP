:- use_module(library(readutil)).

read_file_to_string(Path, String) :-
    read_file_to_string(Path, String, []).
