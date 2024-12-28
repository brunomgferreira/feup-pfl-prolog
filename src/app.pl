:-use_module(library(file_systems)).

menu_path(main, Path):-         Path = './menus/main_menu.txt'. 
menu_path(instructions, Path):- Path = './menus/instructions.txt'. 
menu_path(human_bot, Path):-    Path = './menus/human_bot_menu.txt'. 
menu_path(bot_bot, Path):-      Path = './menus/computer_computer.txt'. 
menu_path(size, Path):-         Path = './menus/board_size_menu.txt'.


% Displays the main menu
main:- 
    display_menu(main),
    repeat,
    read_digit_between(1,5,Value),
    read_specific_char('\n'),
    change_menu(Value, main).


% Displays the instructions
instructions:- 
    display_menu(instructions),
    repeat,
    peek_code(_), skip_line,
    change_menu(_, instructions).


% Displays the human_bot menu
human_bot:-
    display_menu(human_bot),
    repeat,
    read_digit_between(1, 3, Value),
    read_aux(Value, Res),
    change_menu(Res, human_bot).

% Displays the bot_bot menu
bot_bot:-
    display_menu(bot_bot),
    repeat,
    read_digit_between(1, 3, Value),
    read_aux(Value, Res),
    change_menu(Res, bot_bot).

% For later use
read_aux(3, 3):- read_specific_char('\n').
read_aux(V1, V1-V2):- 
    read_specific_char('-'),
    read_digit_between(1, 2, V2),
    read_specific_char('\n').

% Displays the size menu (4x4 and 6x6 are currently disabled) 
size(Mode, Back):-
    display_menu(size),
    repeat,
    read_digit_between(1, 4, Value),
    read_specific_char('\n'),
    (
        Value == 4; 
        Value == 2
    ) ->
    start(Mode, Value, Back);
    writeln('Invalid value. Please enter 4 or 2.'), 
    fail.

exit:- clear_console.

% Displays the menu with the given name

display_menu(Menu):-
    clear_console,
    menu_path(Menu, Path),
    read_from_file(Path).

% Changes the menu according to the given value
change_menu(1, main):- size(['player-player',0], main).
change_menu(2, main):- human_bot.
change_menu(3, main):- bot_bot.
change_menu(4, main):- instructions.
change_menu(5, main):- exit.

% Human_bot game mode
change_menu(3, human_bot):- main.

change_menu(Level-1, human_bot):-  
    size(['computer-player',Level], human_bot).

change_menu(Level-2, human_bot):-  
    size(['player-computer',Level], human_bot).

change_menu(3, bot_bot):- main.


change_menu(Level1-Level2, bot_bot):-  
    size(['computer-computer', Level1, Level2], bot_bot).

change_menu(_, instructions):- main.

% Starts the game with the given mode and size
start(_, 4, Back):- Back.

start(Mode, Value, _):- 
    Size is Value + 3,
    game(Mode, Size),
    prompt_restart_or_menu(Mode, Size).

prompt_restart_or_menu(Mode, Size) :-
    write('Do you want to play again or go back to the main menu?'), nl,
    write('1. Play Again'), nl,
    write('2. Go Back to Main Menu'), nl,
    write('Enter your choice: '),
    repeat,
    read_digit_between(1, 2, Value),
    read_specific_char('\n'),
    handle_choice(Value, Mode, Size).

handle_choice(1, Mode, Size) :-
    game(Mode, Size),
    prompt_restart_or_menu(Mode, Size).

handle_choice(2, _, _) :-
    main.


read_from_file(Path):-
    open(Path, read, Stream),
    print_file(Stream),
    close(Stream),
    nl.


print_file(Stream):-
    peek_code(Stream,-1).

print_file(Stream):-
    get_char(Stream, Char),
    write(Char),
    print_file(Stream).

