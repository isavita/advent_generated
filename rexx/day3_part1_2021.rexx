
/* Rexx */
call main
exit

main:
    fileName = 'input.txt'
    len = 12

    do i = 1 to len
        counts.i.0 = 0
        counts.i.1 = 0
    end

    do while lines(fileName) > 0
        line = linein(fileName)
        do i = 1 to len
            bit = substr(line, i, 1)
            counts.i.bit = counts.i.bit + 1
        end
    end
    call lineout fileName

    gammaRate = 0
    epsilonRate = 0
    do i = 1 to len
        power = 2**(len - i)
        if counts.i.0 > counts.i.1 then
            gammaRate = gammaRate + power
        else
            epsilonRate = epsilonRate + power
    end

    say gammaRate * epsilonRate
return
