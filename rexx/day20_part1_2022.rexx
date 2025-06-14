
/* REXX */
call main
exit

main:
    val. = 0
    pos. = 0
    size = 0
    file = 'input.txt'
    do while lines(file) > 0
        size = size + 1
        val.size = linein(file)
        pos.size = size - 1
    end
    call stream file, 'c', 'close'

    call mix size, 'val.', 'pos.'
    say coords(size, 'val.', 'pos.')
return

mix: procedure expose (pos.) (val.)
    parse arg size, val_stem, pos_stem
    n = size - 1
    if n <= 0 then return

    do i = 1 to size
        oldpos = value(pos_stem || i)
        v = value(val_stem || i)

        newpos = (oldpos + v) // n
        if newpos < 0 then newpos = newpos + n

        if oldpos < newpos then do
            do j = 1 to size
                p_j = value(pos_stem || j)
                if p_j > oldpos & p_j <= newpos then
                    call value pos_stem || j, p_j - 1
            end
        end
        else if newpos < oldpos then do
            do j = 1 to size
                p_j = value(pos_stem || j)
                if p_j >= newpos & p_j < oldpos then
                    call value pos_stem || j, p_j + 1
            end
        end
        call value pos_stem || i, newpos
    end
return

coords: procedure expose (pos.) (val.)
    parse arg size, val_stem, pos_stem
    rev_pos. = ''
    zero_pos = -1

    do i = 1 to size
        p_i = value(pos_stem || i)
        rev_pos.p_i = i
        if value(val_stem || i) = 0 then
            zero_pos = p_i
    end

    coord1 = (zero_pos + 1000) // size
    coord2 = (zero_pos + 2000) // size
    coord3 = (zero_pos + 3000) // size

    idx1 = rev_pos.coord1
    idx2 = rev_pos.coord2
    idx3 = rev_pos.coord3

    sum = value(val_stem || idx1) + value(val_stem || idx2) + value(val_stem || idx3)
return sum
