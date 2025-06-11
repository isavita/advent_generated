
/* Rexx */
call main
exit

main:
    call InitDirections

    i = 0
    do while lines('input.txt') > 0
        i = i + 1
        seating.i = linein('input.txt')
    end
    call stream 'input.txt', 'c', 'close'
    seating.0 = i
    rows = seating.0
    if rows > 0 then cols = length(seating.1)
    else cols = 0

    do forever
        changed = 0
        do i = 1 to rows
            new_seating.i = seating.i
        end

        do r = 1 to rows
            do c = 1 to cols
                char = substr(seating.r, c, 1)
                if char = '.' then iterate

                visible = CountVisibleOccupied(r, c)

                select
                    when char = 'L' & visible = 0 then do
                        new_seating.r = overlay('#', new_seating.r, c)
                        changed = 1
                    end
                    when char = '#' & visible >= 5 then do
                        new_seating.r = overlay('L', new_seating.r, c)
                        changed = 1
                    end
                    otherwise nop
                end
            end
        end

        if \changed then leave

        do i = 1 to rows
            seating.i = new_seating.i
        end
    end

    occupied_count = 0
    do i = 1 to rows
        occupied_count = occupied_count + countstr('#', seating.i)
    end
    say occupied_count
return

CountVisibleOccupied: procedure expose seating. rows cols dx. dy.
    arg row, col
    count = 0
    do d = 1 to 8
        r = row + dy.d
        c = col + dx.d
        do while r > 0 & r <= rows & c > 0 & c <= cols
            seat = substr(seating.r, c, 1)
            if seat = '#' then do
                count = count + 1
                leave
            end
            if seat = 'L' then leave
            r = r + dy.d
            c = c + dx.d
        end
    end
return count

InitDirections: procedure expose dx. dy.
    dx.1 = -1; dy.1 = -1
    dx.2 =  0; dy.2 = -1
    dx.3 =  1; dy.3 = -1
    dx.4 = -1; dy.4 =  0
    dx.5 =  1; dy.5 =  0
    dx.6 = -1; dy.6 =  1
    dx.7 =  0; dy.7 =  1
    dx.8 =  1; dy.8 =  1
return
