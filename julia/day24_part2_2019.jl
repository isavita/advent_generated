
const SIDE = 5
const SQUARE = SIDE * SIDE

function parse()
    res = falses(SQUARE)
    open("input.txt") do file
        for (row, line) in enumerate(eachline(file))
            for col in 1:SIDE
                res[(row-1)*SIDE + col] = line[col] == '#'
            end
        end
    end
    res
end

function infested(space, level, cell)
    haskey(space, level) && space[level][cell]
end

function next2(space)
    newSpace = Dict{Int, BitVector}()
    minLevel, maxLevel = extrema(keys(space))
    for level in minLevel-1:maxLevel+1
        newSpace[level] = falses(SQUARE)
        for cell in 1:SQUARE
            cell == 13 && continue
            row, col = divrem(cell-1, SIDE)
            neighbours = 0
            if row == 0
                neighbours += infested(space, level-1, 8)
            end
            if col == 0
                neighbours += infested(space, level-1, 12)
            end
            if col == SIDE-1
                neighbours += infested(space, level-1, 14)
            end
            if row == SIDE-1
                neighbours += infested(space, level-1, 18)
            end
            if cell == 8
                neighbours += sum(infested(space, level+1, i) for i in 1:SIDE)
            end
            if cell == 12
                neighbours += sum(infested(space, level+1, i*SIDE+1) for i in 0:SIDE-1)
            end
            if cell == 14
                neighbours += sum(infested(space, level+1, i*SIDE+SIDE) for i in 0:SIDE-1)
            end
            if cell == 18
                neighbours += sum(infested(space, level+1, (SIDE-1)*SIDE+i) for i in 1:SIDE)
            end
            if row > 0 && cell != 18
                neighbours += infested(space, level, cell-SIDE)
            end
            if col > 0 && cell != 14
                neighbours += infested(space, level, cell-1)
            end
            if col < SIDE-1 && cell != 12
                neighbours += infested(space, level, cell+1)
            end
            if row < SIDE-1 && cell != 8
                neighbours += infested(space, level, cell+SIDE)
            end
            newSpace[level][cell] = (neighbours == 1) || (neighbours == 2 && !infested(space, level, cell))
        end
    end
    clean!(newSpace)
    newSpace
end

function clean!(space)
    minLevel, maxLevel = extrema(keys(space))
    if all(!, space[minLevel])
        delete!(space, minLevel)
    end
    if all(!, space[maxLevel])
        delete!(space, maxLevel)
    end
end

function main()
    input = parse()
    space = Dict(0 => input)
    for _ in 1:200
        space = next2(space)
    end
    println(sum(sum(grid) for grid in values(space)))
end

main()
