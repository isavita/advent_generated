
/* REXX */
call main
exit

main:
    x = 0
    y = 0
    z = 0
    line = linein('input.txt')

    do until line = ''
        parse var line direction ',' line
        select
            when direction = 'n'  then do; y = y + 1; z = z - 1; end
            when direction = 'ne' then do; x = x + 1; z = z - 1; end
            when direction = 'se' then do; x = x + 1; y = y - 1; end
            when direction = 's'  then do; y = y - 1; z = z + 1; end
            when direction = 'sw' then do; x = x - 1; z = z + 1; end
            when direction = 'nw' then do; x = x - 1; y = y + 1; end
            otherwise nop
        end
    end

    result = max(abs(x), abs(y), abs(z))
    say result
    return
