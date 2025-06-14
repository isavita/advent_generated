
/* REXX */
call main

main:
    SIDE = 5
    SQUARE = SIDE * SIDE
    CENTER_LEVEL = 200
    minLevel = CENTER_LEVEL
    maxLevel = CENTER_LEVEL
    levels. = 0

    call parse 'input.txt'

    do 200
        call next_generation
    end

    total_bugs = 0
    do l = minLevel to maxLevel
        if levels.l.exists then do
            do c = 0 to SQUARE - 1
                total_bugs = total_bugs + levels.l.c
            end
        end
    end
    say total_bugs
return

parse:
    procedure expose levels. CENTER_LEVEL SIDE
    arg filename
    row = 0
    do while lines(filename) > 0
        line = linein(filename)
        do col = 0 to SIDE - 1
            cell = row * SIDE + col
            if substr(line, col + 1, 1) = '#' then
                levels.CENTER_LEVEL.cell = 1
        end
        row = row + 1
    end
    levels.CENTER_LEVEL.exists = 1
    call lineout filename
return

infested:
    procedure expose levels.
    arg level, cell
    if \levels.level.exists then return 0
    return levels.level.cell

next_generation:
    procedure expose levels. minLevel maxLevel SIDE SQUARE
    newLevels. = 0
    newMinLevel = minLevel
    newMaxLevel = maxLevel

    do level = minLevel - 1 to maxLevel + 1
        level_has_bugs = 0
        do cell = 0 to SQUARE - 1
            if cell = 12 then iterate

            row = cell % SIDE
            col = cell // SIDE
            neighbours = 0

            if row = 0 then neighbours = neighbours + infested(level - 1, 7)
            if col = 0 then neighbours = neighbours + infested(level - 1, 11)
            if col = SIDE - 1 then neighbours = neighbours + infested(level - 1, 13)
            if row = SIDE - 1 then neighbours = neighbours + infested(level - 1, 17)

            if cell = 7 then do i=0 to SIDE-1; neighbours=neighbours+infested(level+1, i); end
            if cell = 11 then do i=0 to SIDE-1; neighbours=neighbours+infested(level+1, 5*i); end
            if cell = 13 then do i=0 to SIDE-1; neighbours=neighbours+infested(level+1, 5*i+SIDE-1); end
            if cell = 17 then do i=0 to SIDE-1; neighbours=neighbours+infested(level+1, (SIDE-1)*SIDE+i); end

            if row > 0 & cell \= 17 then neighbours = neighbours + infested(level, cell - SIDE)
            if col > 0 & cell \= 13 then neighbours = neighbours + infested(level, cell - 1)
            if col < SIDE - 1 & cell \= 11 then neighbours = neighbours + infested(level, cell + 1)
            if row < SIDE - 1 & cell \= 7 then neighbours = neighbours + infested(level, cell + SIDE)

            is_infested = infested(level, cell)
            new_state = is_infested
            if is_infested then do
                if neighbours \= 1 then new_state = 0
            end; else do
                if neighbours = 1 | neighbours = 2 then new_state = 1
            end
            newLevels.level.cell = new_state
            if new_state then level_has_bugs = 1
        end

        if level_has_bugs then do
            newLevels.level.exists = 1
            if level < newMinLevel then newMinLevel = level
            if level > newMaxLevel then newMaxLevel = level
        end
    end

    drop levels.
    levels. = 0
    minLevel = newMinLevel
    maxLevel = newMaxLevel
    do l = minLevel to maxLevel
        if newLevels.l.exists then do
            levels.l.exists = 1
            do c = 0 to SQUARE - 1
                levels.l.c = newLevels.l.c
            end
        end
    end

    do while minLevel < maxLevel
        count = 0
        do c = 0 to SQUARE - 1; count = count + levels.minLevel.c; end
        if count > 0 | minLevel = 200 then leave
        levels.minLevel.exists = 0
        minLevel = minLevel + 1
    end

    do while maxLevel > minLevel
        count = 0
        do c = 0 to SQUARE - 1; count = count + levels.maxLevel.c; end
        if count > 0 | maxLevel = 200 then leave
        levels.maxLevel.exists = 0
        maxLevel = maxLevel - 1
    end
return
