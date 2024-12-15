
function solve()
    grid = readlines("input.txt")
    h = length(grid)
    w = length(grid[1])
    antennas = Dict{Char, Vector{Tuple{Int, Int}}}()
    for y in 1:h
        for x in 1:w
            c = grid[y][x]
            if c != '.'
                if !haskey(antennas, c)
                    antennas[c] = []
                end
                push!(antennas[c], (y, x))
            end
        end
    end

    antinodes = Set{Tuple{Int, Int}}()
    for coords in values(antennas)
        n = length(coords)
        for i in 1:n
            for j in i+1:n
                A = coords[i]
                B = coords[j]
                P1 = (2*A[1] - B[1], 2*A[2] - B[2])
                P2 = (2*B[1] - A[1], 2*B[2] - A[2])
                if 1 <= P1[1] <= h && 1 <= P1[2] <= w
                    push!(antinodes, P1)
                end
                if 1 <= P2[1] <= h && 1 <= P2[2] <= w
                    push!(antinodes, P2)
                end
            end
        end
    end
    println(length(antinodes))
end

solve()
