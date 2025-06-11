
/* REXX */
call main
exit

main:
    grid. = 0
    overlapCount = 0
    fileName = 'input.txt'

    do while lines(fileName) > 0
        line = linein(fileName)
        parse var line x1 ',' y1 ' -> ' x2 ',' y2

        if x1 = x2 then do
            do y = min(y1, y2) to max(y1, y2)
                key = x1'.'y
                grid.key = grid.key + 1
                if grid.key = 2 then
                    overlapCount = overlapCount + 1
            end
        end
        else if y1 = y2 then do
            do x = min(x1, x2) to max(x1, x2)
                key = x'.'y1
                grid.key = grid.key + 1
                if grid.key = 2 then
                    overlapCount = overlapCount + 1
            end
        end
    end

    say overlapCount
    return
