
/* REXX */
main:
    call read_input
    say play_game()
exit

read_input:
    boards. = ''
    marked. = 0
    pos. = ''
    numbers. = ''

    line = linein('input.txt')
    line = space(changestr(',', line, ' '), 1)
    num_count = 0
    do while line <> ''
        num_count = num_count + 1
        parse var line numbers.num_count line
    end
    numbers.0 = num_count

    board_count = 0
    do while lines('input.txt') > 0
        call linein 'input.txt' /* Skip blank line */
        if stream('input.txt', 'S') = 'ERROR' then leave
        board_count = board_count + 1
        do r = 1 to 5
            line = linein('input.txt')
            do c = 1 to 5
                parse var line num line
                boards.board_count.r.c = num
                pos.board_count.num = r c
            end
        end
    end
    boards.0 = board_count
return

play_game:
    do n = 1 to numbers.0
        num = numbers.n
        do b = 1 to boards.0
            if symbol('pos.b.'num) == 'VAR' then do
                parse var pos.b.num r c
                marked.b.r.c = 1
                if check_win(b, r, c) then
                    return calculate_unmarked_sum(b) * num
            end
        end
    end
return

check_win: procedure expose marked.
    arg board, row, col
    row_win = 1
    col_win = 1
    do i = 1 to 5
        row_win = row_win & marked.board.row.i
        col_win = col_win & marked.board.i.col
    end
return row_win | col_win

calculate_unmarked_sum: procedure expose boards. marked.
    arg board
    sum = 0
    do r = 1 to 5
        do c = 1 to 5
            if marked.board.r.c = 0 then
                sum = sum + boards.board.r.c
        end
    end
return sum
