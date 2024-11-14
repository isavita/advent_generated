
using DelimitedFiles

struct P
    x::Int
    y::Int
end

@enum Dir N=0 E=1 S=2 W=3

function Base.:(+)(d::Dir, x::Int)
    Dir(mod(Int(d) + x, 4))
end

function Base.:(-)(d::Dir, x::Int)
    Dir(mod(Int(d) - x + 4, 4))
end

function points(d::Dir)
    mod(Int(d) + 3, 4)
end

function rotate(d::Dir, direction::Char)
    if direction == 'R'
        return d + 1
    elseif direction == 'L'
        return d - 1
    end
    return d
end

struct Movement
    steps::Int
    rotate::Char
end

mutable struct Human
    curr::P
    facing::Dir
end

const DIRS = [P(-1, 0), P(0, 1), P(1, 0), P(0, -1)]

function parse_input()
    lines = readlines("input.txt")
    size = length(lines[1]) ÷ 3
    map = Dict{P, Bool}()
    
    for (r, line) in enumerate(lines)
        if line == ""
            break
        end
        
        for (c, char) in enumerate(line)
            if char == '#'
                map[P(r-1, c-1)] = true
            elseif char == '.'
                map[P(r-1, c-1)] = false
            end
        end
    end
    
    movements = parse_path(lines[findfirst(x -> x == "", lines) + 1])
    
    return map, size, movements
end

function parse_path(path::String)
    movements = Movement[]
    acc = 0
    
    for char in path
        if char == 'R'
            push!(movements, Movement(acc, ' '))
            acc = 0
            push!(movements, Movement(0, 'R'))
        elseif char == 'L'
            push!(movements, Movement(acc, ' '))
            acc = 0
            push!(movements, Movement(0, 'L'))
        else
            acc = 10 * acc + parse(Int, char)
        end
    end
    
    push!(movements, Movement(acc, ' '))
    return movements
end

function walk!(human::Human, map::Dict{P, Bool})
    dir = DIRS[Int(human.facing) + 1]
    
    next = P(human.curr.x + dir.x, human.curr.y + dir.y)
    
    if haskey(map, next)
        if map[next]
            return false
        end
        human.curr = next
        return true
    end
    
    # fast-forward in opposite direction
    opp_dir = P(-dir.x, -dir.y)
    while true
        look_ahead = P(next.x + opp_dir.x, next.y + opp_dir.y)
        
        if !haskey(map, look_ahead)
            if map[next]
                return false
            end
            human.curr = next
            return true
        end
        next = look_ahead
    end
end

function solve()
    map, size, movements = parse_input()
    
    human = Human(P(0, size), E)
    
    for mov in movements
        if mov.rotate != ' '
            human.facing = rotate(human.facing, mov.rotate)
        end
        
        for _ in 1:mov.steps
            if !walk!(human, map)
                break
            end
        end
    end
    
    println(1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + points(human.facing))
end

solve()
