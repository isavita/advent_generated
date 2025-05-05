
const U    = ComplexF64(0, -1)
const D  = ComplexF64(0, 1)
const L = ComplexF64(-1, 0)
const R = ComplexF64(1, 0)

function parse(input::String)
    blocks = split(strip(input), "\n\n")
    lines = split(blocks[1], "\n")
    grid = Dict{ComplexF64, Char}()
    for y = 0:length(lines)-1
        line = lines[y+1]
        for x = 0:length(line)-1
            grid[ComplexF64(x, y)] = line[x+1]
        end
    end
    steps_str = replace(blocks[2], "\n" => "")
    steps = ComplexF64[]
    for ch in steps_str
        if ch == '^'
            push!(steps, U)
        elseif ch == '<'
            push!(steps, L)
        elseif ch == '>'
            push!(steps, R)
        elseif ch == 'v'
            push!(steps, D)
        end
    end
    return grid, steps
end

function scale(input::String)
    s = input
    s = replace(s, "#" => "##")
    s = replace(s, "." => "..")
    s = replace(s, "O" => "[]")
    s = replace(s, "@" => "@.")
    return s
end

function can_enter(m::Dict{ComplexF64, Char}, target::ComplexF64, dir::ComplexF64)::Union{Dict{ComplexF64, Char}, Nothing}
    val = get(m, target, '#')
    if val == '.'
        return copy(m)
    elseif val == 'O' || val == '@'
        next_m = can_enter(m, target + dir, dir)
        if !isnothing(next_m)
            next_m[target + dir] = val
            next_m[target] = '.'
            return next_m
        end
    elseif val == ']'
        return can_push(m, target + L, dir)
    elseif val == '['
         return can_push(m, target, dir)
    end
    return nothing
end

function can_push(m::Dict{ComplexF64, Char}, obj_pos::ComplexF64, dir::ComplexF64)::Union{Dict{ComplexF64, Char}, Nothing}
    val = get(m, obj_pos, '#')
    if val == 'O' || val == '@'
        next_m = can_enter(m, obj_pos + dir, dir)
        if !isnothing(next_m)
            next_m[obj_pos + dir] = val
            next_m[obj_pos] = '.'
            return next_m
        end
    elseif val == '['
        if dir == L
            next_m = can_enter(m, obj_pos + L, L)
            if !isnothing(next_m)
                 next_m[obj_pos + L] = '['
                 next_m[obj_pos] = ']'
                 next_m[obj_pos + R] = '.'
                 return next_m
            end
        elseif dir == R
            next_m = can_enter(m, obj_pos + 2 * R, R)
            if !isnothing(next_m)
                 next_m[obj_pos] = '.'
                 next_m[obj_pos + R] = '['
                 next_m[obj_pos + 2 * R] = ']'
                 return next_m
            end
        else
            m1 = can_enter(m, obj_pos + dir, dir)
            if !isnothing(m1)
                 m2 = can_enter(m1, obj_pos + R + dir, dir)
                 if !isnothing(m2)
                     next_m = copy(m2)
                     next_m[obj_pos] = '.'
                     next_m[obj_pos + R] = '.'
                     next_m[obj_pos + dir] = '['
                     next_m[obj_pos + dir + R] = ']'
                     return next_m
                 end
            end
        end
    end
    return nothing
end

function try_move(m::Dict{ComplexF64, Char}, robot_pos::ComplexF64, dir::ComplexF64)::Union{Tuple{Dict{ComplexF64, Char}, ComplexF64}, Nothing}
    target = robot_pos + dir
    val = get(m, target, '#')
    if val == '.'
        next_m = copy(m)
        next_m[target] = '@'
        next_m[robot_pos] = '.'
        return next_m, target
    elseif val == 'O' || val == '[' || val == ']'
        obj_pos = target
        if val == ']'
             obj_pos = target + L
        end
        next_m = can_push(m, obj_pos, dir)
        if !isnothing(next_m)
            next_m[target] = '@'
            next_m[robot_pos] = '.'
            return next_m, target
        end
    end
    return nothing
end

function solve(grid::Dict{ComplexF64, Char}, steps::Vector{ComplexF64})
    robot_pos = ComplexF64(0, 0)
    for (k, v) in grid
        if v == '@'
            robot_pos = k
            break
        end
    end
    m = copy(grid)
    for dir in steps
        result = try_move(m, robot_pos, dir)
        if !isnothing(result)
            m, robot_pos = result
        end
    end

    score = 0.0
    for (k, v) in m
        if v == '[' || v == 'O'
            score += real(k) + 100.0 * imag(k)
        end
    end
    return score
end

function main()
    input = read("input.txt", String)
    grid1, steps1 = parse(input)
    result1 = solve(grid1, steps1)
    println(Int(trunc(result1)))
    scaled_input = scale(input)
    grid2, steps2 = parse(scaled_input)
    result2 = solve(grid2, steps2)
    println(Int(trunc(result2)))
end

main()
