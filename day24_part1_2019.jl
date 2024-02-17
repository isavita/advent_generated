
const Side = 5
const Square = Side * Side

function parse()
    res = falses(Square)

    file = open("input.txt")
    for row in 1:Side
        line = readline(file)
        for col in 1:Side
            if line[col] == '#'
                res[(row-1)*Side+col] = true
            end
        end
    end
    close(file)
    return res
end

function main()
    appeared = Dict{Array{Bool,1}, Nothing}()

    grid = parse()
    appeared[grid] = nothing
    while true
        grid = next1(grid)
        if haskey(appeared, grid)
            println(biodiversity(grid))
            return
        end
        appeared[grid] = nothing
    end
end

function next1(grid)
    newGrid = falses(Square)

    for i in 1:Square
        row, col = div(i-1, Side)+1, mod(i-1, Side)+1
        neighbours = 0

        if row > 1 && grid[i-Side]
            neighbours += 1
        end
        if row < Side && grid[i+Side]
            neighbours += 1
        end
        if col > 1 && grid[i-1]
            neighbours += 1
        end
        if col < Side && grid[i+1]
            neighbours += 1
        end

        if grid[i] && neighbours != 1
            newGrid[i] = false
            continue
        end

        if !grid[i] && (neighbours == 1 || neighbours == 2)
            newGrid[i] = true
            continue
        end

        newGrid[i] = grid[i]
    end

    return newGrid
end

function biodiversity(grid)
    bio = 0
    for i in 1:Square
        if grid[i]
            bio += 2^(i-1)
        end
    end
    return bio
end

main()
