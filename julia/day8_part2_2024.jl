
function gcd(a::Int, b::Int)::Int
    while b != 0
        a, b = b, a % b
    end
    return abs(a)
end

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
                    antennas[c] = Vector{Tuple{Int, Int}}()
                end
                push!(antennas[c], (y, x))
            end
        end
    end

    lines_per_freq = Dict{Char, Set{String}}()
    for (f, coords) in antennas
        lines_per_freq[f] = Set{String}()
        n = length(coords)
        for i in 1:n
            for j in i+1:n
                A = coords[i]
                B = coords[j]
                dy = B[1] - A[1]
                dx = B[2] - A[2]
                g = gcd(dy, dx)
                sy = dy ÷ g
                sx = dx ÷ g
                if sx < 0 || (sx == 0 && sy < 0)
                    sx = -sx
                    sy = -sy
                end
                c = sy * A[2] - sx * A[1]
                key = string(sx) * "," * string(sy) * "," * string(c)
                push!(lines_per_freq[f], key)
            end
        end
    end

    antinodes = Set{Tuple{Int, Int}}()
    for lines in values(lines_per_freq)
        for key in lines
            sx, sy, c = parse.(Int, split(key, ","))
            if sx == 0 && sy == 0
                continue
            end
            if sy == 0
                if c % sx == 0
                    y = -c ÷ sx
                    if 1 <= y <= h
                        for x in 1:w
                            push!(antinodes, (y, x))
                        end
                    end
                end
            elseif sx == 0
                if c % sy == 0
                    x = c ÷ sy
                    if 1 <= x <= w
                        for y in 1:h
                            push!(antinodes, (y, x))
                        end
                    end
                end
            else
                for y in 1:h
                    val = c + sx * y
                    if val % sy == 0
                        x = val ÷ sy
                        if 1 <= x <= w
                            push!(antinodes, (y, x))
                        end
                    end
                end
            end
        end
    end
    println(length(antinodes))
end

solve()
