% play()
% initial_state(GameConfig, GameState)
% display_game(GameState)
% move(GameState, Move, NewGameState)
% valid_moves(GameState, ListOfMoves)
% game_over(GameState, Winner)
% value(GameState, Player, Value)
% choose_move(GameState, Level, Move)

read_number(X):-
    read_number_aux(X,0).
read_number_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X,X).

clear_console:-
    write('\33\[2J').

print_white_square :-
    write('\e[47m'),
    write('\e[97m'),
    write(' '),
    write(' '),
    write('\e[0m').

print_black_square :-
    write('\e[40m'),
    write('\e[30m'),
    write(' '),
    write(' '),
    write('\e[0m').

print_empty_square :-
    write('\e[48;5;239m'),
    write('\e[38;5;239m'),
    write(' '),
    write(' '),
    write('\e[0m').

board(
    [
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ]]).

print_board(Board) :-
    print_empty_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_empty_square,
    nl,
    print_board_aux(Board),
    print_empty_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_white_square,
    print_empty_square,
    nl.

print_board_aux([]).
print_board_aux([Row | Board]) :-
    print_black_square,
    print_board_row(Row),
    print_black_square,
    nl,
    print_board_aux(Board).

print_board_row([]).
print_board_row([X | Row]) :-
    (X = ['empty', _] -> print_empty_square ;
     X = ['white', _] -> print_white_square ;
     X = ['black', _] -> print_black_square),
    print_board_row(Row).

show_board :-
    board(Board),
    print_board(Board),
    get_code(C).