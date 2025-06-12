
/* REXX */
main:
    call solve
exit

solve:
    fname = 'input.txt'
    parse value linein(fname) with input_data
    call linein fname, 1, 0

    numCups = length(input_data)
    maxCup = 9
    minCup = 1

    do i = 1 to numCups
        currentCup = substr(input_data, i, 1)
        nextPos = i // numCups + 1
        nextCup = substr(input_data, nextPos, 1)
        Next.currentCup = nextCup
    end

    current = substr(input_data, 1, 1)
    moves = 100

    do move = 1 to moves
        p1 = Next.current
        p2 = Next.p1
        p3 = Next.p2

        destination = current - 1
        do forever
            if destination < minCup then destination = maxCup
            if destination \= p1 & destination \= p2 & destination \= p3 then leave
            destination = destination - 1
        end

        after_p3 = Next.p3
        Next.current = after_p3

        after_dest = Next.destination
        Next.destination = p1
        Next.p3 = after_dest

        current = Next.current
    end

    result = ''
    ptr = Next.1
    do i = 1 to numCups - 1
        result = result || ptr
        ptr = Next.ptr
    end

    say result
return
