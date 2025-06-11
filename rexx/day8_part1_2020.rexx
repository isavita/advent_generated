
/* Rexx Main Entry Point */
call main
exit

main:
    fileName = 'input.txt'
    i = 0
    do while lines(fileName) > 0
        i = i + 1
        instructions.i = linein(fileName)
    end
    instructions.0 = i
    call stream fileName, 'C', 'CLOSE'

    accumulator = 0
    ip = 1
    drop visited.

    do forever
        if symbol('visited.'ip) == 'VAR' then leave
        visited.ip = 1
        parse var instructions.ip op arg
        select
            when op = 'acc' then do
                accumulator = accumulator + arg
                ip = ip + 1
            end
            when op = 'jmp' then ip = ip + arg
            when op = 'nop' then ip = ip + 1
        end
    end

    say accumulator
return
