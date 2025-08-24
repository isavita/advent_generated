const directions = Dict(
    "e" => (1, 0),
    "se" => (0, 1),
    "sw" => (-1, 1),
    "w" => (-1, 0),
    "nw" => (0, -1),
    "ne" => (1, -1)
)

blackTiles = Dict{Tuple{Int,Int},Bool}()

open("input.txt") do file
    for line in eachline(file)
        q, r = 0, 0
        i = 1
        while i <= length(line)
            if line[i] in ('e', 'w')
                dir = line[i:i]
                i += 1
            else
                dir = line[i:i+1]
                i += 2
            end
            dq, dr = directions[dir]
            q += dq
            r += dr
        end
        blackTiles[(q, r)] = !get(blackTiles, (q, r), false)
    end
end

count = sum(values(blackTiles))
println(count)