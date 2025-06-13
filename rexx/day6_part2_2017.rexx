
/* REXX */
call main
exit

main:
    line = linein('input.txt')
    num_banks = words(line)
    do i = 1 to num_banks
        banks.i = word(line, i)
    end

    seen. = ''
    cycles = 0

    do forever
        key = banks.1
        do i = 2 to num_banks
            key = key'.'banks.i
        end

        if datatype(seen.key, 'W') then leave
        seen.key = cycles

        max_val = -1
        do i = 1 to num_banks
            if banks.i > max_val then do
                max_val = banks.i
                max_idx = i
            end
        end

        blocks = banks.max_idx
        banks.max_idx = 0
        idx = max_idx
        do j = 1 for blocks
            idx = idx + 1
            if idx > num_banks then idx = 1
            banks.idx = banks.idx + 1
        end

        cycles = cycles + 1
    end

    say cycles
    say cycles - seen.key
    return
