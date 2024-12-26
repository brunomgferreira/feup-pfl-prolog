:- use_module(library(lists)).

replace(Index, Element, List, Result) :-
  nth0(Index, List, _, R),
  nth0(Index, Result, Element, R).

max_list([X], X).
max_list([X, Y | Rest], Max) :-
    max_list([Y | Rest], MaxRest),
    Max is max(X, MaxRest).

get_line(Result, Acc):-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    get_line(Result, Acc1), !.
get_line(Result, Acc):-
    atom_chars(Result, Acc), !.

clear_buffer:-
    repeat,
    get_char(C),
    C = '\n'.

clear_console:-
    write('\33\[2J').

set_color_white :-
    write('\e[47m'),
    write('\e[90m').

set_color_black :-
    write('\e[40m'),
    write('\e[90m').

set_color_white_valid :-
    write('\e[48;5;12m'),
    write('\e[38;5;18m').

set_color_black_valid :-
    write('\e[48;5;18m'),
    write('\e[38;5;12m').

set_color_white_invalid :-
    write('\e[48;5;9m'),
    write('\e[38;5;1m').

set_color_black_invalid :-
    write('\e[48;5;1m'),
    write('\e[38;5;9m').

set_color_grey :-
    write('\e[48;5;239m'),
    write('\e[38;5;250m').

reset_color :-
    write('\e[0m').

write_grey(S) :-
    set_color_grey,
    write(S),
    reset_color.

write_white(S) :-
    set_color_white,
    write(S),
    reset_color.

write_black(S) :-
    set_color_black,
    write(S),
    reset_color.

write_valid(S) :-
    set_color_white_valid,
    write(S),
    reset_color.

write_invalid(S) :-
    set_color_white_invalid,
    write(S),
    reset_color.

read_specific_char(Value):-
    get_char(Char),
    Value == Char, !.

read_specific_char(_):-
    skip_line,
    write('Invalid input!\n'), fail.
    
% Reads a digit between Min and Max
read_digit_between(Min, Max, Value):-
    read_digit(X),
    between(Min, Max, X),!,
    Value is X.

read_digit_between(_, _, _):-
    skip_line, 
    write('Invalid input!\n'), fail.    

read_digit(_):-
    peek_code(Value),
    \+between(48, 57, Value), !, fail.

read_digit(Value):-
    get_code(Ascii),
    char_code('0', AsciiZero),
    Value is Ascii - AsciiZero.