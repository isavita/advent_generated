
/* REXX */
main:
    fname = 'input.txt'
    stacks. = ''
    drawing_lines. = ''
    drawing_lines.0 = 0

    do i = 1 while lines(fname) > 0
        line = linein(fname)
        if line = '' then leave
        drawing_lines.i = line
        drawing_lines.0 = i
    end

    num_stacks = (length(drawing_lines.1) + 1) % 4
    do i = drawing_lines.0 - 1 to 1 by -1
        line = drawing_lines.i
        do j = 1 to num_stacks
            pos = 2 + (j - 1) * 4
            if pos <= length(line) then do
                crate = substr(line, pos, 1)
                if datatype(crate, 'U') then
                    stacks.j = stacks.j || crate
            end
        end
    end

    do while lines(fname) > 0
        line = linein(fname)
        if line = '' then iterate
        parse var line 'move' n 'from' from 'to' to
        n = strip(n); from = strip(from); to = strip(to)

        crates_to_move = right(stacks.from, n)
        stacks.to = stacks.to || crates_to_move
        stacks.from = left(stacks.from, length(stacks.from) - n)
    end
    call close fname

    result = ''
    do i = 1 to num_stacks
        if stacks.i \= '' then
            result = result || right(stacks.i, 1)
    end
    say result
return
