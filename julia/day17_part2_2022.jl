
function main()
    jet_pattern = read("input.txt", String)
    rocks = [
        [(0, 0), (1, 0), (2, 0), (3, 0)],
        [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
        [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
        [(0, 0), (0, 1), (0, 2), (0, 3)],
        [(0, 0), (1, 0), (0, 1), (1, 1)],
    ]
    grid = Set([(x, 0) for x in 0:6])
    floor, j = 0, 1
    repeat = Dict{Tuple{Int,Int},Tuple{Int,Int}}()
    target = 1000000000000
    i = 0
    curr = 1
    while true
        key = (curr, j)
        if haskey(repeat, key)
            previ, prev_floor = repeat[key]
            if (target - i) % (i - previ) == 0
                println(floor + (target - i) ÷ (i - previ) * (floor - prev_floor))
                break
            end
        end
        repeat[key] = (i, floor)
        curr_rock = rocks[curr]
        pos = (2, floor + 4)
        while true
            jet = jet_pattern[j]
            j = mod1(j + 1, length(jet_pattern))
            next_pos = (pos[1] + (jet == '>' ? 1 : -1), pos[2])
            if !collision(grid, curr_rock, next_pos)
                pos = next_pos
            end
            next_pos = (pos[1], pos[2] - 1)
            if collision(grid, curr_rock, next_pos)
                for p in curr_rock
                    np = (p[1] + pos[1], p[2] + pos[2])
                    push!(grid, np)
                    floor = max(floor, np[2])
                end
                break
            end
            pos = next_pos
        end
        i += 1
        curr = mod1(curr + 1, length(rocks))
    end
end

function collision(grid, rock, pos)
    for p in rock
        np = (p[1] + pos[1], p[2] + pos[2])
        if np in grid || np[1] < 0 || np[1] > 6
            return true
        end
    end
    false
end

main()
