function count_houses(path::String)
    houses = Dict{Tuple{Int, Int}, Int}()
    x, y = 0, 0
    houses[(x, y)] = 1
    for move in path
        if move == '>'
            x += 1
        elseif move == '<'
            x -= 1
        elseif move == '^'
            y += 1
        elseif move == 'v'
            y -= 1
        end
        houses[(x, y)] = get(houses, (x, y), 0) + 1
    end
    return length(houses)
end

path = read("input.txt", String)
println(count_houses(path))
