
/* REXX */
call main

main:
    preambleLength = 25
    inputFile = 'input.txt'
    i = 0
    do while lines(inputFile) > 0
        i = i + 1
        numbers.i = linein(inputFile)
    end
    numbers.0 = i
    call stream inputFile, 'C', 'CLOSE'

    do i = preambleLength + 1 to numbers.0
        if \isValid(numbers.i, i - preambleLength, i - 1) then do
            say numbers.i
            exit
        end
    end
return

isValid: procedure expose numbers.
    arg target, startIdx, endIdx
    do j = startIdx to endIdx
        do k = j + 1 to endIdx
            if numbers.j + numbers.k = target then
                return 1
        end
    end
    return 0
