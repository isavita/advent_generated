
using DelimitedFiles

struct Point
    x::Int
    y::Int
end

function move(p::Point, dir::Char)
    if dir == 'N'
        return Point(p.x, p.y - 1)
    elseif dir == 'S'
        return Point(p.x, p.y + 1)
    elseif dir == 'E'
        return Point(p.x + 1, p.y)
    elseif dir == 'W'
        return Point(p.x - 1, p.y)
    else
        error("Invalid direction")
    end
end

function build_map(regex)
    dm = Dict{Point,Dict{Point,Bool}}()
    stack = Point[]
    cp = Point(0, 0)
    for c in regex
        if c == '('
            push!(stack, cp)
        elseif c == '|'
            cp = stack[end]
        elseif c == ')'
            cp = pop!(stack)
        else
            np = move(cp, c)
            if !haskey(dm, cp)
                dm[cp] = Dict{Point,Bool}()
            end
            if !haskey(dm, np)
                dm[np] = Dict{Point,Bool}()
            end
            dm[cp][np] = true
            dm[np][cp] = true
            cp = np
        end
    end
    return dm
end

function count_rooms(dm, min_doors)
    visited = Dict{Point,Int}()
    queue = Point[]
    push!(queue, Point(0, 0))
    room_count = 0

    while !isempty(queue)
        p = popfirst!(queue)
        for np in keys(dm[p])
            if !haskey(visited, np)
                visited[np] = get(visited, p, 0) + 1
                if visited[np] >= min_doors
                    room_count += 1
                end
                push!(queue, np)
            end
        end
    end

    return room_count
end

data = read("input.txt", String)
regex = data[2:end-1]
dm = build_map(regex)
rooms = count_rooms(dm, 1000)
println(rooms)
