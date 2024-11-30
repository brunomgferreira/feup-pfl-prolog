board(
    [
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['white', 1], ['black', 1], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['black', 1], ['white', 1], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ]]).

final_board(
    [
    [ ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 3], ['empty', 4], ['empty', 4], ['empty', 4], ['empty', 4], ['empty', 3], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 3], ['empty', 4], ['white', 4], ['black', 4], ['empty', 4], ['empty', 3], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 3], ['empty', 4], ['black', 4], ['white', 4], ['empty', 4], ['empty', 3], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 3], ['empty', 4], ['empty', 4], ['empty', 4], ['empty', 4], ['empty', 3], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1] ]]).


possible_moves([[1, 1], [1, 2], [1, 3], [1, 4], [1, 5], [1, 6], [1, 7], [1, 8], [1, 9],
                [2, 1], [2, 2], [2, 3], [2, 4], [2, 5], [2, 6], [2, 7], [2, 8], [2, 9],
                [3, 1], [3, 2], [3, 3], [3, 4], [3, 5], [3, 6], [3, 7], [3, 8], [3, 9],
                [4, 1], [4, 2], [4, 3], [4, 4], [4, 5], [4, 6], [4, 7], [4, 8], [4, 9],
                [5, 1], [5, 2], [5, 3], [5, 4], [5, 5], [5, 6], [5, 7], [5, 8], [5, 9],
                [6, 1], [6, 2], [6, 3], [6, 4], [6, 5], [6, 6], [6, 7], [6, 8], [6, 9],
                [7, 1], [7, 2], [7, 3], [7, 4], [7, 5], [7, 6], [7, 7], [7, 8], [7, 9],
                [8, 1], [8, 2], [8, 3], [8, 4], [8, 5], [8, 6], [8, 7], [8, 8], [8, 9],
                [9, 1], [9, 2], [9, 3], [9, 4], [9, 5], [9, 6], [9, 7], [9, 8], [9, 9]]).

initial_valid_moves([[1, 1], [1, 3], [1, 5], [1, 7], [1, 9],
                     [3, 1], [3, 3], [3, 5], [3, 7], [3, 9],
                     [5, 1], [5, 3],         [5, 7], [5, 9],
                     [7, 1], [7, 3], [7, 5], [7, 7], [7, 9],
                     [9, 1], [9, 3], [9, 5], [9, 7], [9, 9]]).

final_valid_moves([[5,5]]).