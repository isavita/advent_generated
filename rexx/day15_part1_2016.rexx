
call main
exit

main:
    fname = 'input.txt'
    do i = 1 while lines(fname) > 0
        line = linein(fname)
        parse var line 'Disc #' . ' has ' d.i.total ' positions; at time=0, it is at position ' d.i.start '.'
        d.0 = i
    end

    time = 0
    step = 1
    do i = 1 to d.0
        do forever
            if (d.i.start + time + i) // d.i.total = 0 then leave
            time = time + step
        end
        step = step * d.i.total
    end

    say time
return
