
/* Rexx */
call main
exit

main:
    fileName = 'input.txt'
    count = 0
    do while lines(fileName) > 0
        jumps.count = linein(fileName)
        count = count + 1
    end

    index = 0
    steps = 0

    do while index >= 0 & index < count
        offset = jumps.index
        if offset < 3 then
            jumps.index = jumps.index + 1
        else
            jumps.index = jumps.index - 1
        index = index + offset
        steps = steps + 1
    end

    say steps
return
