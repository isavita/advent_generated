
function find_position(mat, ch)
    for (i, row) in enumerate(mat)
        for (j, cell) in enumerate(row)
            if cell == ch
                return (i, j)
            end
        end
    end
    return (-1, -1)
end

function ok(mat, st, seq)
    curr = [st[1], st[2]]
    for ch in seq
        if mat[curr[1]][curr[2]] == ' '
            return false
        end
        if ch == '^'
            curr[1] -= 1
        elseif ch == 'v'
            curr[1] += 1
        elseif ch == '<'
            curr[2] -= 1
        elseif ch == '>'
            curr[2] += 1
        end
        if curr[1] < 1 || curr[1] > length(mat) || curr[2] < 1 || curr[2] > length(mat[1])
            return false
        end
    end
    return true
end

function generate_moves(position, objective, pad, move_cache)
    key = (position, objective, join(pad))
    if haskey(move_cache, key)
        return move_cache[key]
    end
    obj_pos = find_position(pad, objective)
    ret = ""
    if position[2] > obj_pos[2]
        ret *= "<"^(position[2] - obj_pos[2])
    end
    if position[1] > obj_pos[1]
        ret *= "^"^(position[1] - obj_pos[1])
    end
    if position[1] < obj_pos[1]
        ret *= "v"^(obj_pos[1] - position[1])
    end
    if position[2] < obj_pos[2]
        ret *= ">"^(obj_pos[2] - position[2])
    end
    if !ok(pad, position, ret)
        ret = ""
        if position[2] < obj_pos[2]
            ret *= ">"^(obj_pos[2] - position[2])
        end
        if position[1] > obj_pos[1]
            ret *= "^"^(position[1] - obj_pos[1])
        end
        if position[1] < obj_pos[1]
            ret *= "v"^(obj_pos[1] - position[1])
        end
        if position[2] > obj_pos[2]
            ret *= "<"^(position[2] - obj_pos[2])
        end
    end
    move_cache[key] = ret
    return ret
end

function solve(code, robots, key_pad, robot_pad, max_robots, solve_cache, move_cache)
    key = (code, robots, max_robots)
    if haskey(solve_cache, key)
        return solve_cache[key]
    end
    if robots <= 0
        return length(code)
    end
    ret = 0
    posi, posj = 4, 3
    if robots != max_robots
        posi = 1
    end
    for ch in code
        if robots == max_robots
            moves = generate_moves((posi, posj), ch, key_pad, move_cache)
            pos = find_position(key_pad, ch)
            posi, posj = pos[1], pos[2]
        else
            moves = generate_moves((posi, posj), ch, robot_pad, move_cache)
            pos = find_position(robot_pad, ch)
            posi, posj = pos[1], pos[2]
        end
        ret += solve(moves * "A", robots - 1, key_pad, robot_pad, max_robots, solve_cache, move_cache)
    end
    solve_cache[key] = ret
    return ret
end

function main()
    content = read("input.txt", String)
    max_robots = 26
    key_pad = ["789", "456", "123", " 0A"]
    robot_pad = [" ^A", "<v>"]
    ret = 0
    codes = split(strip(content), "\n")
    solve_cache = Dict()
    move_cache = Dict()
    for code in codes
        code = strip(code)
        if code == ""
            continue
        end
        numeric_part = 0
        for ch in code
            if '0' <= ch <= '9'
                numeric_part = numeric_part * 10 + Int(ch - '0')
            end
        end
        sv = solve(code, max_robots, key_pad, robot_pad, max_robots, solve_cache, move_cache)
        ret += sv * numeric_part
    end
    println(ret)
end

main()
