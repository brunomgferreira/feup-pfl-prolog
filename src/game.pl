:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(board).
:- consult(utils).
:- consult(display).

% play
play :-
    initial_state('TODO - GameConfig', GameState),
    get_move(GameState, _),
    display_game(GameState).

% initial_state(GameConfig, GameState)
initial_state(_, [Board, Player, 27, 27, ValidMoves]) :-
    board(Board),
    valid_moves(ValidMoves),
    random_member(Player, ['white', 'black']).

% display_game(GameState)
display_game([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves]) :-
    clear_console,
    print_header(WhiteBlocks, BlackBlocks),
    print_board(Board),
    print_player_turn(Player),
    print_valid_moves(ValidMoves).

% move(GameState, Move, NewGameState)
% valid_moves(GameState, ListOfMoves)
% game_over(GameState, Winner)
% value(GameState, Player, Value)
% choose_move(GameState, Level, Move)

get_move([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], Move) :-
    (length(ValidMoves, 0) -> Move = [], ! ; 
    nth0(0, ValidMoves, CurrentMove),
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], CurrentMove, Move, 1)).

get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col], Move, Direction) :-
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
        get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, NewCol], Move, Direction),
        !
    ; (Code = 68 ; Code = 100) ->  % D - right
        (Col = 9 -> NewCol = 1 ; NewCol is (Col + 1) mod 10),
        get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, NewCol], Move, Direction),
        !
    ; (Code = 87 ; Code = 119) ->  % W - up
        (Row = 9 -> NewRow = 1 ; NewRow is (Row + 1) mod 10),
        get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [NewRow, Col], Move, Direction),
        !
    ; (Code = 83 ; Code = 115) ->  % S - down
        (Row = 1 -> NewRow = 9 ; NewRow is (Row - 1) mod 10),
        get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [NewRow, Col], Move, Direction),
        !
    ; (Code = 82 ; Code = 114) ->  % R - Rotate
        (Direction = 1 -> NewDirection = 2 ; Direction = 2 -> NewDirection = 1),
        get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col], Move, NewDirection),
        !
    ; ((Code = 67 ; Code = 99), ValidMove) ->  % C - Confirm
        Move = [Row, Col],
        !
    ; ((Code = 67 ; Code = 99), \+ ValidMove) ->  % C - Confirm
        get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col], Move, Direction),
        !
    ; get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col], Move, Direction)
    ).
