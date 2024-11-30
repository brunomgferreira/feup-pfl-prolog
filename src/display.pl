:- consult(utils).

% ---------------------------------------------
% ------------- Aux Functions -----------------
% ---------------------------------------------

print_grey_line :-
    write_grey('                                    '),
    nl.

print_white_square(C) :-
    set_color_white,
    write(' '),
    write(C),
    write(' '),
    reset_color.

print_black_square(C) :-
    set_color_black,
    write(' '),
    write(C),
    write(' '),
    reset_color.

print_white_valid_square(C) :-
    set_color_white_valid,
    write(' '),
    write(C),
    write(' '),
    reset_color.

print_black_valid_square(C) :-
    set_color_black_valid,
    write(' '),
    write(C),
    write(' '),
    reset_color.

print_white_invalid_square(C) :-
    set_color_white_invalid,
    write(' '),
    write(C),
    write(' '),
    reset_color.

print_black_invalid_square(C) :-
    set_color_black_invalid,
    write(' '),
    write(C),
    write(' '),
    reset_color.

print_empty_square(C) :-
    set_color_grey,
    write(' '),
    write(C),
    write(' '),
    reset_color.

% ---------------------------------------------
% ---------------- Board ----------------------
% ---------------------------------------------

print_board(Board) :-
    nl,
    print_empty_square(' '),
    set_color_white,
    write('                              '),
    set_color_grey,
    print_empty_square(' '),
    reset_color,
    nl,
    print_board_aux(Board, 0),
    print_empty_square(' '),
    set_color_white,
    write(' 1  2  3  4  5  6  7  8  9  0 '),
    set_color_grey,
    print_empty_square(' '),
    reset_color,
    nl.

print_board_aux([], _).
print_board_aux([Row | Board], 0) :-
    print_black_square(0),
    print_board_row(Row),
    print_black_square(' '),
    nl,
    print_board_aux(Board, 9),
    !.
print_board_aux([Row | Board], RowIndex) :-
    print_black_square(RowIndex),
    print_board_row(Row),
    print_black_square(' '),
    nl,
    NewRowIndex is RowIndex - 1,
    print_board_aux(Board, NewRowIndex),
    !.

print_board_row([]).
print_board_row([['empty', Height] | Row]) :- print_empty_square(Height), print_board_row(Row).
print_board_row([['white', Height] | Row]) :- print_white_square(Height), print_board_row(Row).
print_board_row([['black', Height] | Row]) :- print_black_square(Height), print_board_row(Row).
print_board_row([['white_valid', Height] | Row]) :- print_white_valid_square(Height), print_board_row(Row).
print_board_row([['black_valid', Height] | Row]) :- print_black_valid_square(Height), print_board_row(Row).
print_board_row([['white_invalid', Height] | Row]) :- print_white_invalid_square(Height), print_board_row(Row).
print_board_row([['black_invalid', Height] | Row]) :- print_black_invalid_square(Height), print_board_row(Row).

% ---------------------------------------------
% ------------- Player Turn -------------------
% ---------------------------------------------

print_player_turn('white') :-
    nl,
    print_white_square(' '),
    write_grey('      White Player Turn!      '),
    print_white_square(' '),
    nl. 
print_player_turn('black') :-
    nl,
    print_black_square(' '),
    write_grey('      Black Player Turn!      '),
    print_black_square(' '),
    nl.

% ---------------------------------------------
% ------------- Number Blocks -----------------
% ---------------------------------------------

print_number_blocks(WhiteBlocks, BlackBlocks) :-
    nl,
    print_grey_line,
    write_grey('     --     Blocks  Left     --     '),
    nl,
    print_grey_line,
    print_empty_square(' '),
    print_black_square(' '),
    print_white_square(' '),
    ( WhiteBlocks < 10 
        ->  write_grey('     '),
            write_grey(WhiteBlocks),
            write_grey('  White'),
            write_grey('     ')
    ; WhiteBlocks >= 10
        ->  write_grey('     '),
            write_grey(WhiteBlocks),
            write_grey(' White'),
            write_grey('     ')),
    print_white_square(' '),
    print_black_square(' '),
    print_empty_square(' '),
    nl,
    print_empty_square(' '),
    print_black_square(' '),
    print_white_square(' '),
    ( WhiteBlocks = 1 
        ->  write_grey('      '),
            write_grey('Block '),
            write_grey('      ')
    ; WhiteBlocks \= 1
        ->  write_grey('      '),
            write_grey('Blocks'),
            write_grey('      ')),
    print_white_square(' '),
    print_black_square(' '),
    print_empty_square(' '),
    nl,
    print_grey_line,
    print_empty_square(' '),
    print_black_square(' '),
    print_black_square(' '),
    ( BlackBlocks < 10 
        ->  write_grey('     '),
            write_grey(BlackBlocks),
            write_grey('  Black'),
            write_grey('     ')
    ; BlackBlocks >= 10 
        ->  write_grey('     '),
            write_grey(BlackBlocks),
            write_grey(' Black'),
            write_grey('     ')),
    print_white_square(' '),
    print_white_square(' '),
    print_empty_square(' '),
    nl,
    print_empty_square(' '),
    print_white_square(' '),
    print_white_square(' '),
    ( BlackBlocks = 1 
        ->  write_grey('      '),
            write_grey('Block '),
            write_grey('      ')
    ; BlackBlocks \= 1
        ->  write_grey('      '),
            write_grey('Blocks'),
            write_grey('      ')),
    print_black_square(' '),
    print_black_square(' '),
    print_empty_square(' '),
    nl,
    print_grey_line.

% ---------------------------------------------
% ------------- Valid Moves -------------------
% ---------------------------------------------

print_valid_moves(ValidMoves) :-
    print_grey_line,
    write_grey('     --     Valid  Moves     --     '),
    nl,
    print_grey_line,
    print_valid_moves_aux(ValidMoves).


print_valid_moves_aux([]) :-
    print_grey_line,
    !.
print_valid_moves_aux([[Row1, Col1]]) :-
    write_grey('    '),
    write_grey(Row1),
    write_grey('-'),
    write_grey(Col1),
    write_grey('                             '),
    nl,
    print_valid_moves_aux([]),
    !.
print_valid_moves_aux([[Row1, Col1], [Row2, Col2]]) :-
    write_grey('    '),
    write_grey(Row1),
    write_grey('-'),
    write_grey(Col1),
    write_grey(' | '),
    write_grey(Row2),
    write_grey('-'),
    write_grey(Col2),
    write_grey('                       '),
    nl,
    print_valid_moves_aux([]),
    !.
print_valid_moves_aux([[Row1, Col1], [Row2, Col2], [Row3, Col3]]) :-
    write_grey('    '),
    write_grey(Row1),
    write_grey('-'),
    write_grey(Col1),
    write_grey(' | '),
    write_grey(Row2),
    write_grey('-'),
    write_grey(Col2),
    write_grey(' | '),
    write_grey(Row3),
    write_grey('-'),
    write_grey(Col3),
    write_grey('                 '),
    nl,
    print_valid_moves_aux([]),
    !.
print_valid_moves_aux([[Row1, Col1], [Row2, Col2], [Row3, Col3], [Row4, Col4]]) :-
    write_grey('    '),
    write_grey(Row1),
    write_grey('-'),
    write_grey(Col1),
    write_grey(' | '),
    write_grey(Row2),
    write_grey('-'),
    write_grey(Col2),
    write_grey(' | '),
    write_grey(Row3),
    write_grey('-'),
    write_grey(Col3),
    write_grey(' | '),
    write_grey(Row4),
    write_grey('-'),
    write_grey(Col4),
    write_grey('           '),
    nl,
    print_valid_moves_aux([]),
    !.
print_valid_moves_aux([[Row1, Col1], [Row2, Col2], [Row3, Col3], [Row4, Col4], [Row5, Col5] | ValidMoves]) :-
    write_grey('    '),
    write_grey(Row1),
    write_grey('-'),
    write_grey(Col1),
    write_grey(' | '),
    write_grey(Row2),
    write_grey('-'),
    write_grey(Col2),
    write_grey(' | '),
    write_grey(Row3),
    write_grey('-'),
    write_grey(Col3),
    write_grey(' | '),
    write_grey(Row4),
    write_grey('-'),
    write_grey(Col4),
    write_grey(' | '),
    write_grey(Row5),
    write_grey('-'),
    write_grey(Col5),
    write_grey('     '),
    nl,
    print_valid_moves_aux(ValidMoves), 
    !.