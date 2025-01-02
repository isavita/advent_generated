
function solve()
    lines = readlines("input.txt")
    graph = [split(line, "") for line in lines if line != ""]
    H, W = length(graph), length(graph[1])

    moves = [
        ("left", -1, 0),
        ("up", 0, -1),
        ("right", 1, 0),
        ("down", 0, 1),
    ]

    function save_outer!(label, side, x, y)
        key = (label == "up" || label == "down") ? "$y:$x" : "$x:$y"
        if !haskey(side, label)
            side[label] = Set{String}()
        end
        push!(side[label], key)
    end

    function count_outer(side)
        outer = 0
        for label in keys(side)
            arr = sort(collect(side[label]), by=x -> begin
                i, j = parse.(Int, split(x, ":"))
                (i, j)
            end)
            
            temp = String[]
            for current in arr
                i, j = parse.(Int, split(current, ":"))
                if !any(s -> s ∈ temp, ["$i:$(j-1)", "$i:$(j+1)"])
                    outer += 1
                end
                push!(temp, current)
            end
        end
        outer
    end

    total_sum = 0

    for y in 1:H
        for x in 1:W
            if graph[y][x] == "."
                continue
            end

            area = 0
            target = graph[y][x]
            visited = Set{Tuple{Int, Int}}()
            side = Dict{String, Set{String}}()

            function search(cx, cy, label)
                if graph[cy][cx] != target
                    if label != "" && !((cx, cy) ∈ visited)
                        save_outer!(label, side, cx, cy)
                    end
                    return
                end

                push!(visited, (cx, cy))
                area += 1
                graph[cy][cx] = "."

                for (m_label, dx, dy) in moves
                    nx, ny = cx + dx, cy + dy

                    if nx < 1 || nx > W || ny < 1 || ny > H
                        save_outer!(m_label, side, nx, ny)
                        continue
                    end
                    search(nx, ny, m_label)
                end
            end

            search(x, y, "")
            outer = count_outer(side)
            total_sum += area * outer
        end
    end
    println(total_sum)
end

solve()
