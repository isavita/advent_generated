
function solve()
    rules = Dict{String, String}()
    open("input.txt") do file
        for line in eachline(file)
            parts = split(line, " => ")
            rules[parts[1]] = parts[2]
        end
    end

    grid = [".#.", "..#", "###"]
    for _ in 1:18
        n = length(grid)
        if n % 2 == 0
            subsize = 2
            newsize = n ÷ 2 * 3
        else
            subsize = 3
            newsize = n ÷ 3 * 4
        end
        newgrid = fill("", newsize)
        for y in 1:subsize:n
            for x in 1:subsize:n
                square = [grid[y+dy-1][x:x+subsize-1] for dy in 1:subsize]
                newsquare = enhance(join(square, "/"), rules)
                for (dy, row) in enumerate(split(newsquare, "/"))
                    newgrid[(y-1)÷subsize*(subsize+1)+dy] *= row
                end
            end
        end
        grid = newgrid
    end
    count = sum(sum(c == '#' for c in row) for row in grid)
    println(count)
end

memo = Dict{String, String}()

function enhance(input, rules)
    get!(memo, input) do
        original = input
        for _ in 1:4
            if haskey(rules, input)
                return rules[input]
            end
            input = rotate(input)
        end
        input = flip(input)
        for _ in 1:4
            if haskey(rules, input)
                return rules[input]
            end
            input = rotate(input)
        end
        return ""
    end
end

function rotate(input)
    parts = split(input, "/")
    n = length(parts)
    newparts = ["" for _ in 1:n]
    for x in 1:n
        for y in n:-1:1
            newparts[x] *= parts[y][x]
        end
    end
    return join(newparts, "/")
end

function flip(input)
    parts = split(input, "/")
    return join(reverse.(parts), "/")
end

solve()
