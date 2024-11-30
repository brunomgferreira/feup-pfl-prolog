:- use_module(library(lists)).

replace(Index, Element, List, Result) :-
  nth0(Index, List, _, R),
  nth0(Index, Result, Element, R).

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
