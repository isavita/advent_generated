
const favoriteNumber = 1362

struct Point
    x::Int
    y::Int
end

function isWall(x, y)
    num = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
    bits = 0
    while num > 0
        if num % 2 == 1
            bits += 1
        end
        num = div(num, 2)
    end
    return bits % 2 != 0
end

function bfsMaxSteps(start::Point, maxSteps::Int)
    visited = Dict{Point, Bool}()
    queue = [start]
    visited[start] = true
    steps = 0

    while !isempty(queue) && steps < maxSteps
        size = length(queue)
        for i in 1:size
            point = queue[i]

            for delta in [Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1)]
                next = Point(point.x + delta.x, point.y + delta.y)
                if next.x >= 0 && next.y >= 0 && !isWall(next.x, next.y) && !haskey(visited, next)
                    visited[next] = true
                    push!(queue, next)
                end
            end
        end
        queue = queue[size+1:end]
        steps += 1
    end

    return length(keys(visited))
end

start = Point(1, 1)
reachableLocations = bfsMaxSteps(start, 50)
println(reachableLocations)
