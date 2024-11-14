
struct Point
    x::Int
    y::Int
end

struct State
    pos::Point
    keys::Int
end

function findShortestPath(grid::Vector{String}, start::Point, keyMap::Dict{Char,Int})
    dirs = [Point(0, -1), Point(-1, 0), Point(0, 1), Point(1, 0)]
    visited = Set{State}()
    queue = [State(start, 0)]
    steps = 0

    while !isempty(queue)
        size = length(queue)
        for _ in 1:size
            current = popfirst!(queue)

            if current.keys == (1 << length(keyMap)) - 1
                return steps
            end

            for d in dirs
                next = Point(current.pos.x + d.x, current.pos.y + d.y)
                if 1 <= next.x <= length(grid[1]) && 1 <= next.y <= length(grid)
                    char = grid[next.y][next.x]
                    if char != '#' && !(char in 'A':'Z' && (current.keys & (1 << keyMap[char - 'A' + 'a'])) == 0)
                        newState = State(next, current.keys)
                        if char in 'a':'z'
                            newState = State(newState.pos, newState.keys | (1 << keyMap[char]))
                        end
                        if newState ∉ visited
                            push!(visited, newState)
                            push!(queue, newState)
                        end
                    end
                end
            end
        end
        steps += 1
    end
    return -1
end

function main()
    grid = readlines("input.txt")
    start = Point(0, 0)
    keyMap = Dict{Char,Int}()
    keyCounter = 0

    for y in 1:length(grid)
        line = grid[y]
        for x in 1:length(line)
            if line[x] == '@'
                start = Point(x, y)
            elseif line[x] in 'a':'z'
                keyMap[line[x]] = keyCounter
                keyCounter += 1
            end
        end
    end

    println(findShortestPath(grid, start, keyMap))
end

main()
