:- use_module(library(lists)).
:- consult(utils).
:- consult(display).
:- consult(data).

put_piece(Board, Row, Col, Color, NewBoard) :-
    RowIndex is 10 - Row,
    ColIndex is Col - 1,
    nth0(RowIndex, Board, Line),
    nth0(ColIndex, Line, [_, Height]),
    NewHeight is Height + 1,
    replace(ColIndex, [Color, NewHeight], Line, NewLine),
    replace(RowIndex, NewLine, Board, NewBoard).

put_block(Board, Row, Col, 'white', NewBoard, 1) :- put_white_block_direction_1(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'white', NewBoard, 2) :- put_white_block_direction_2(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'black', NewBoard, 1) :- put_black_block_direction_1(Board, Row, Col, NewBoard). 
put_block(Board, Row, Col, 'black', NewBoard, 2) :- put_black_block_direction_2(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'white_valid', NewBoard, 1) :- put_white_valid_block_direction_1(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'white_valid', NewBoard, 2) :- put_white_valid_block_direction_2(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'black_valid', NewBoard, 1) :- put_black_valid_block_direction_1(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'black_valid', NewBoard, 2) :- put_black_valid_block_direction_2(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'white_invalid', NewBoard, 1) :- put_white_invalid_block_direction_1(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'white_invalid', NewBoard, 2) :- put_white_invalid_block_direction_2(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'black_invalid', NewBoard, 1) :- put_black_invalid_block_direction_1(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'black_invalid', NewBoard, 2) :- put_black_invalid_block_direction_2(Board, Row, Col, NewBoard).

put_white_block_direction_1(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'white', TempBoard1),
    put_piece(TempBoard1, Row2, Col, 'white', TempBoard2),
    put_piece(TempBoard2, Row, Col2, 'black', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'black', NewBoard).

put_white_block_direction_2(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'black', TempBoard1),
    put_piece(TempBoard1, Row2, Col, 'black', TempBoard2),
    put_piece(TempBoard2, Row, Col2, 'white', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'white', NewBoard).
    
put_black_block_direction_1(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'black', TempBoard1),
    put_piece(TempBoard1, Row, Col2, 'black', TempBoard2),
    put_piece(TempBoard2, Row2, Col, 'white', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'white', NewBoard).

put_black_block_direction_2(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'white', TempBoard1),
    put_piece(TempBoard1, Row, Col2, 'white', TempBoard2),
    put_piece(TempBoard2, Row2, Col, 'black', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'black', NewBoard).

put_white_valid_block_direction_1(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'white_valid', TempBoard1),
    put_piece(TempBoard1, Row2, Col, 'white_valid', TempBoard2),
    put_piece(TempBoard2, Row, Col2, 'black_valid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'black_valid', NewBoard).

put_white_valid_block_direction_2(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'black_valid', TempBoard1),
    put_piece(TempBoard1, Row2, Col, 'black_valid', TempBoard2),
    put_piece(TempBoard2, Row, Col2, 'white_valid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'white_valid', NewBoard).
    
put_black_valid_block_direction_1(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'black_valid', TempBoard1),
    put_piece(TempBoard1, Row, Col2, 'black_valid', TempBoard2),
    put_piece(TempBoard2, Row2, Col, 'white_valid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'white_valid', NewBoard).

put_black_valid_block_direction_2(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'white_valid', TempBoard1),
    put_piece(TempBoard1, Row, Col2, 'white_valid', TempBoard2),
    put_piece(TempBoard2, Row2, Col, 'black_valid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'black_valid', NewBoard).

put_white_invalid_block_direction_1(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'white_invalid', TempBoard1),
    put_piece(TempBoard1, Row2, Col, 'white_invalid', TempBoard2),
    put_piece(TempBoard2, Row, Col2, 'black_invalid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'black_invalid', NewBoard).

put_white_invalid_block_direction_2(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'black_invalid', TempBoard1),
    put_piece(TempBoard1, Row2, Col, 'black_invalid', TempBoard2),
    put_piece(TempBoard2, Row, Col2, 'white_invalid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'white_invalid', NewBoard).
    
put_black_invalid_block_direction_1(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'black_invalid', TempBoard1),
    put_piece(TempBoard1, Row, Col2, 'black_invalid', TempBoard2),
    put_piece(TempBoard2, Row2, Col, 'white_invalid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'white_invalid', NewBoard).

put_black_invalid_block_direction_2(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'white_invalid', TempBoard1),
    put_piece(TempBoard1, Row, Col2, 'white_invalid', TempBoard2),
    put_piece(TempBoard2, Row2, Col, 'black_invalid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'black_invalid', NewBoard).