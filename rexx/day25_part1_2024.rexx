
/* REXX */
call main
exit

main:
    fname = 'input.txt'
    BlockHeight = 7
    ShapeWidth = 5
    ShapeHeight = 6
    num_locks = 0
    num_keys = 0
    total_valid_lines = 0
    lines_in_block = 0
    block_invalid = 0
    drop block.

    do while lines(fname) > 0
        line = linein(fname)
        if strip(line) == '' then iterate

        total_valid_lines = total_valid_lines + 1
        lines_in_block = lines_in_block + 1
        block.lines_in_block = line

        if length(strip(line, 'T')) < ShapeWidth then block_invalid = 1

        if lines_in_block = BlockHeight then do
            if \block_invalid then call process_block
            lines_in_block = 0
            block_invalid = 0
            drop block.
        end
    end
    call stream fname, 'C', 'CLOSE'

    if total_valid_lines = 0 | total_valid_lines // BlockHeight \= 0 then do
        say 0
        return
    end

    count = 0
    do i = 1 to num_locks
        do j = 1 to num_keys
            if fits(i, j) then count = count + 1
        end
    end
    say count
return

process_block:
    if verify(substr(block.1, 1, ShapeWidth), '#') = 0 then do
        num_locks = num_locks + 1
        call parse_item 'lock', num_locks
    end
    else do
        num_keys = num_keys + 1
        call parse_item 'key', num_keys
    end
return

parse_item:
    parse arg type, idx
    do c = 1 to ShapeWidth
        cnt = 0
        if type = 'lock' then do
            do r = 2 to BlockHeight
                if substr(block.r, c, 1) \= '#' then leave
                cnt = cnt + 1
            end
            locks.idx.c = cnt
        end
        else do
            do r = ShapeHeight to 1 by -1
                if substr(block.r, c, 1) \= '#' then leave
                cnt = cnt + 1
            end
            keys.idx.c = cnt
        end
    end
return

fits: procedure expose locks. keys. ShapeWidth ShapeHeight
    parse arg l_idx, k_idx
    do c = 1 to ShapeWidth
        if locks.l_idx.c + keys.k_idx.c > ShapeHeight - 1 then return 0
    end
    return 1
