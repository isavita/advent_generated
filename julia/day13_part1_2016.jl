
struct Point
    x::Int
    y::Int
end

function isWall(favoriteNumber, x, y)
    num = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
    bits = 0
    while num > 0
        if num % 2 == 1
            bits += 1
        end
        num ÷= 2
    end
    return bits % 2 != 0
end

function bfs(start, target, favoriteNumber)
    visited = Dict()
    queue = [start]
    steps = 0

    while !isempty(queue)
        size = length(queue)
        for i in 1:size
            point = queue[i]
            if point == target
                return steps
            end

            for delta in [(1, 0), (-1, 0), (0, 1), (0, -1)]
                next = Point(point.x + delta[1], point.y + delta[2])
                if next.x >= 0 && next.y >= 0 && !isWall(favoriteNumber, next.x, next.y) && !haskey(visited, next)
                    visited[next] = true
                    push!(queue, next)
                end
            end
        end
        queue = queue[size+1:end]
        steps += 1
    end

    return -1
end

data = read("input.txt", String)
favoriteNumber = parse(Int, strip(data))
start = Point(1, 1)
target = Point(31, 39)
steps = bfs(start, target, favoriteNumber)
println(steps)
