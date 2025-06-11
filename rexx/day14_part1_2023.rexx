
/* REXX */
call main
exit

main:
    file = 'input.txt'
    height = 0
    do y = 1 while lines(file) > 0
        line = linein(file)
        if y = 1 then width = length(line)
        do x = 1 to width
            grid.y.x = substr(line, x, 1)
        end
        height = y
    end
    call lineout file

    do x = 1 to width
        land_y = 1
        do y = 1 to height
            select
                when grid.y.x = 'O' then do
                    grid.y.x = '.'
                    grid.land_y.x = 'O'
                    land_y = land_y + 1
                end
                when grid.y.x = '#' then
                    land_y = y + 1
                otherwise
                    nop
            end
        end
    end

    total_load = 0
    do y = 1 to height
        do x = 1 to width
            if grid.y.x = 'O' then
                total_load = total_load + (height - y + 1)
        end
    end

    say total_load
return
