:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(board).
:- consult(utils).
:- consult(display).

% play
play :-
    initial_state('TODO - GameConfig', GameState),
    game_cycle('TODO - GameConfig', GameState, '').

% initial_state(GameConfig, GameState)
initial_state(_, [Board, Player, 27, 27, ValidMoves]) :-
    final_board(Board),
    final_valid_moves(ValidMoves),
    random_member(Player, ['white', 'black']).

% display_game(GameState)
display_game([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves]) :-
    clear_console,
    print_header(WhiteBlocks, BlackBlocks),
    print_board(Board),
    print_player_turn(Player),
    print_valid_moves(ValidMoves).

game_cycle(_, [Board, _, WhiteBlocks, BlackBlocks, _], 'white') :- 
    clear_console,
    print_header(WhiteBlocks, BlackBlocks),
    print_board(Board),
    print_winner_message('white'),
    !.
game_cycle(_, [Board, _, WhiteBlocks, BlackBlocks, _], 'black') :-
    clear_console,
    print_header(WhiteBlocks, BlackBlocks),
    print_board(Board),
    print_winner_message('black'),
    !.
game_cycle(GameConfig, [Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], _) :-
    get_move([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], Move),
    move([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], Move, [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves]),
    game_over([NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner),
    game_cycle(GameConfig, [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner).

% move(GameState, Move, NewGameState)
move([Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], NewGameState) :-
    ValidMove = member([Row, Col], ValidMoves),
    (ValidMove ->
        put_block(Board, Row, Col, 'white', NewBoard, Direction),
        NewWhiteBlocks is WhiteBlocks - 1,
        valid_moves([NewBoard, 'black', NewWhiteBlocks, BlackBlocks, []], NewValidMoves),
        NewGameState = [NewBoard, 'black', NewWhiteBlocks, BlackBlocks, NewValidMoves] ;
     \+ ValidMove ->
        NewGameState is [Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves]).
move([Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], NewGameState) :-
    ValidMove = member([Row, Col], ValidMoves),
    (ValidMove ->
        put_block(Board, Row, Col, 'black', NewBoard, Direction),
        NewBlackBlocks is BlackBlocks - 1,
        valid_moves([NewBoard, 'white', WhiteBlocks, NewBlackBlocks, []], NewValidMoves),
        NewGameState = [NewBoard, 'white', WhiteBlocks, NewBlackBlocks, NewValidMoves] ;
     \+ ValidMove ->
        NewGameState is [Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves]).

% valid_moves(GameState, ListOfMoves)
valid_moves([Board, _, _, _, _], ListOfMoves) :-
    valid_moves_aux(Board, Board, 10, [], ReversedListOfMoves),
    reverse(ReversedListOfMoves, ListOfMoves).

valid_moves_aux(_, [],  _, AccumulatedMoves, AccumulatedMoves) :- !.
valid_moves_aux(Board, [Line | Rest], Row, AccumulatedMoves, ListOfMoves) :-
    valid_moves_row(Board, Line, Row, 1, AccumulatedMoves, NewAccumulatedMoves),
    NewRow is Row - 1,
    valid_moves_aux(Board, Rest, NewRow, [], NewListOfMoves),
    append(NewAccumulatedMoves, NewListOfMoves, ListOfMoves).

valid_moves_row(_, [], _, _, AccumulatedMoves, AccumulatedMoves) :- !.
valid_moves_row(_, _, 10, _, AccumulatedMoves, AccumulatedMoves) :- !.
valid_moves_row(_, _, _, 10, AccumulatedMoves, AccumulatedMoves) :- !.
valid_moves_row(Board, [[_, Height] | Line], Row, Col, AccumulatedMoves, ListOfMoves) :-
    Row < 10,
    Col < 10,
    NewCol is Col + 1,
    RowIndex is 10 - Row,
    ColIndex is Col - 1,
    RowIndex2 is RowIndex - 1,
    ColIndex2 is ColIndex + 1,
    nth0(RowIndex, Board, Temp),
    nth0(ColIndex2, Temp, [_, Height2]),
    nth0(RowIndex2, Board, Temp2),
    nth0(ColIndex, Temp2, [_, Height3]),
    nth0(ColIndex2, Temp2, [_, Height4]),
    ((Height =:= 0, Row mod 2 =:= 1, Col mod 2 =:= 1) ->
        valid_moves_row(Board, Line, Row, NewCol, [[Row, Col] | AccumulatedMoves], ListOfMoves)
    ;
    (Height =:= 1, Height2 =:= 1, Height3 =:= 1, Height4 =:= 1, Row > 1, Row < 9, Col > 1, Col < 9, Row mod 2 =:= 0, Col mod 2 =:= 0) ->
        valid_moves_row(Board, Line, Row, NewCol, [[Row, Col] | AccumulatedMoves], ListOfMoves), !
    ;
    (Height =:= 2, Height2 =:= 2, Height3 =:= 2, Height4 =:= 2, Row > 2, Row < 8, Col > 2, Col < 8, Row mod 2 =:= 1, Col mod 2 =:= 1) ->
        valid_moves_row(Board, Line, Row, NewCol, [[Row, Col] | AccumulatedMoves], ListOfMoves), !
    ;
    (Height =:= 3, Height2 =:= 3, Height3 =:= 3, Height4 =:= 3, Row > 3, Row < 7, Col > 3, Col < 7, Row mod 2 =:= 0, Col mod 2 =:= 0) ->
        valid_moves_row(Board, Line, Row, NewCol, [[Row, Col] | AccumulatedMoves], ListOfMoves), !
    ;
    (Height =:= 4, Height2 =:= 4, Height3 =:= 4, Height4 =:= 4, Row =:= 5, Col =:= 5) ->
        valid_moves_row(Board, Line, Row, NewCol, [[Row, Col] | AccumulatedMoves], ListOfMoves), !
    ;
        valid_moves_row(Board, Line, Row, NewCol, AccumulatedMoves, ListOfMoves), !
    ).

% game_over(GameState, Winner)
game_over([_, 'white', _, _, []], 'black') :- !.
game_over([_, 'black', _, _, []], 'white') :- !.
game_over(_, '') :- !. % TODO

% initial_state(_, GameState), valid_moves(GameState, ListOfMoves).
% value(GameState, Player, Value)
% choose_move(GameState, Level, Move)

get_move([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], Move) :-
    (length(ValidMoves, 0) -> Move = [], ! ; 
    nth0(0, ValidMoves, [Row, Col]),
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, 1], Move)).

get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], Move) :-
    ValidMove = member([Row, Col], ValidMoves),
    (ValidMove -> 
        (Player = 'white' -> 
            put_block(Board, Row, Col, 'white_valid', TempBoard, Direction) ;
         Player = 'black' ->
            put_block(Board, Row, Col, 'black_valid', TempBoard, Direction)) ;
     \+ ValidMove ->
        (Player = 'white' -> 
            put_block(Board, Row, Col, 'white_invalid', TempBoard, Direction) ;
         Player = 'black' ->
            put_block(Board, Row, Col, 'black_invalid', TempBoard, Direction))),
    display_game([TempBoard, Player, WhiteBlocks, BlackBlocks, ValidMoves]),
    (ValidMove -> print_valid_move_message ;
    \+ ValidMove -> print_invalid_move_message),
    get_code(Code),
    clear_buffer,
    ((Code = 65 ; Code = 97) ->  % A - left
        (Col = 1 -> NewCol = 9 ; NewCol is (Col - 1) mod 10),
        get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, NewCol, Direction], Move),
        !
    ; (Code = 68 ; Code = 100) ->  % D - right
        (Col = 9 -> NewCol = 1 ; NewCol is (Col + 1) mod 10),
        get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, NewCol, Direction], Move),
        !
    ; (Code = 87 ; Code = 119) ->  % W - up
        (Row = 9 -> NewRow = 1 ; NewRow is (Row + 1) mod 10),
        get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [NewRow, Col, Direction], Move),
        !
    ; (Code = 83 ; Code = 115) ->  % S - down
        (Row = 1 -> NewRow = 9 ; NewRow is (Row - 1) mod 10),
        get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [NewRow, Col, Direction], Move),
        !
    ; (Code = 82 ; Code = 114) ->  % R - Rotate
        (Direction = 1 -> NewDirection = 2 ; Direction = 2 -> NewDirection = 1),
        get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, NewDirection], Move),
        !
    ; ((Code = 67 ; Code = 99), ValidMove) ->  % C - Confirm
        Move = [Row, Col, Direction],
        !
    ; ((Code = 67 ; Code = 99), \+ ValidMove) ->  % C - Confirm
        get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], Move),
        !
    ; get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], Move)
    ).
