
/* Rexx solution for the Dumbo Octopus problem */
call main
exit

main:
    call readInput 'input.txt'
    step = 0
    do forever
        step = step + 1
        flashes = simulateStep()
        if flashes = 100 then leave
    end
    say step
return

readInput:
    procedure expose grid.
    parse arg fname
    grid.0.h = 0
    do while lines(fname) > 0
        grid.0.h = grid.0.h + 1
        y = grid.0.h
        line = linein(fname)
        grid.0.w = length(line)
        do x = 1 to grid.0.w
            grid.y.x = substr(line, x, 1)
        end
    end
    call stream fname, 'c', 'close'
return

simulateStep:
    procedure expose grid.
    flashed. = 0
    q. = ''
    q.0 = 0

    do y = 1 to grid.0.h
        do x = 1 to grid.0.w
            grid.y.x = grid.y.x + 1
        end
    end

    do y = 1 to grid.0.h
        do x = 1 to grid.0.w
            if grid.y.x > 9 & flashed.y.x = 0 then do
                call flash y, x
            end
        end
    end

    totalFlashes = 0
    do y = 1 to grid.0.h
        do x = 1 to grid.0.w
            if flashed.y.x = 1 then do
                totalFlashes = totalFlashes + 1
                grid.y.x = 0
            end
        end
    end
return totalFlashes

flash:
    procedure expose grid. flashed.
    parse arg y, x
    if flashed.y.x then return 0
    flashed.y.x = 1

    do dy = -1 to 1
        do dx = -1 to 1
            if dx = 0 & dy = 0 then iterate
            ny = y + dy
            nx = x + dx
            if ny > 0 & ny <= grid.0.h & nx > 0 & nx <= grid.0.w then do
                grid.ny.nx = grid.ny.nx + 1
                if grid.ny.nx > 9 then do
                    call flash ny, nx
                end
            end
        end
    end
return
