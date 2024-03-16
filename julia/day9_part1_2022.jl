open("input.txt") do f
    lines = readlines(f)
    head = [0, 0]
    tail = [0, 0]
    visited = Set{Tuple{Int,Int}}()
    push!(visited, tuple(tail...))
    for line in lines
        dir, steps = split(line)
        steps = parse(Int, steps)
        for _ in 1:steps
            if dir == "R"
                head[1] += 1
            elseif dir == "L"
                head[1] -= 1
            elseif dir == "U"
                head[2] += 1
            else
                head[2] -= 1
            end
            if abs(head[1] - tail[1]) > 1 || abs(head[2] - tail[2]) > 1
                if head[1] > tail[1]
                    tail[1] += 1
                elseif head[1] < tail[1]
                    tail[1] -= 1
                end
                if head[2] > tail[2]
                    tail[2] += 1
                elseif head[2] < tail[2]
                    tail[2] -= 1
                end
                push!(visited, tuple(tail...))
            end
        end
    end
    println(length(visited))
end