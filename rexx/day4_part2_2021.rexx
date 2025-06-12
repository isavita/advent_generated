
/* REXX */
main:
    call parse_input

    last_winning_score = -1
    drop already_won.

    do n = 1 to nums.0
        num = nums.n
        do bi = 1 to boards.0
            if symbol('ALREADY_WON.'bi) == 'VAR' then iterate

            if pick_and_check(bi, num) then do
                score = calculate_score(bi)
                last_winning_score = score * num
                already_won.bi = 1
            end
        end
    end

    say last_winning_score
exit

parse_input:
    file = 'input.txt'
    drop nums. boards. picked.

    line = translate(linein(file), ' ', ',')
    do i = 1 to words(line)
        nums.i = word(line, i)
    end
    nums.0 = i - 1

    call linein file /* Skip blank line after numbers */

    bi = 0
    do while lines(file) > 0
        bi = bi + 1
        board_size = 0
        do r = 1 by 1
            line = linein(file)
            if line = '' then leave
            board_size = r
            line = space(line, 1)
            do c = 1 to words(line)
                boards.bi.r.c = word(line, c)
                picked.bi.r.c = 0
            end
            boards.bi.r.0 = c - 1
        end
        boards.bi.0 = board_size
    end
    boards.0 = bi
    call stream file, 'c', 'close'
return

pick_and_check: procedure expose boards. picked.
    parse arg bi, num
    board_size = boards.bi.0
    do r = 1 to board_size
        do c = 1 to boards.bi.r.0
            if boards.bi.r.c = num then do
                picked.bi.r.c = 1
                is_row_win = 1
                is_col_win = 1
                do k = 1 to board_size
                    if picked.bi.r.k = 0 then is_row_win = 0
                    if picked.bi.k.c = 0 then is_col_win = 0
                end
                if is_row_win | is_col_win then return 1
                return 0
            end
        end
    end
return 0

calculate_score: procedure expose boards. picked.
    parse arg bi
    score = 0
    do r = 1 to boards.bi.0
        do c = 1 to boards.bi.r.0
            if picked.bi.r.c = 0 then
                score = score + boards.bi.r.c
        end
    end
return score
