
function solve()
    robots = Vector{Vector{Int}}()
    open("input.txt") do file
        for line in eachline(file)
            parts = split(line, " ")
            pPart = replace(parts[1], "p=" => "")
            vPart = replace(parts[2], "v=" => "")
            pos = split(pPart, ",")
            vel = split(vPart, ",")
            px = parse(Int, pos[1])
            py = parse(Int, pos[2])
            vx = parse(Int, vel[1])
            vy = parse(Int, vel[2])
            push!(robots, [px, py, vx, vy])
        end
    end

    width, height = 101, 103
    for _ in 1:100
        for r in robots
            x = (r[1] + r[3]) % width
            y = (r[2] + r[4]) % height
            if x < 0 x += width end
            if y < 0 y += height end
            r[1] = x
            r[2] = y
        end
    end

    q1, q2, q3, q4 = 0, 0, 0, 0
    for r in robots
        x, y = r[1], r[2]
        if x == 50 || y == 51 continue end
        if x < 50 && y < 51 q1 += 1 end
        if x > 50 && y < 51 q2 += 1 end
        if x < 50 && y > 51 q3 += 1 end
        if x > 50 && y > 51 q4 += 1 end
    end

    println(q1 * q2 * q3 * q4)
end

solve()
