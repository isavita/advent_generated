
function solve()
    rules = Dict{String, String}()
    
    open("input.txt", "r") do file
        for line in eachline(file)
            parts = split(line, " => ")
            rules[parts[1]] = parts[2]
        end
    end
    
    grid = [".#.", "..#", "###"]
    
    for _ in 1:5
        subsize = length(grid) % 2 == 0 ? 2 : 3
        newsize = subsize == 2 ? length(grid) ÷ 2 * 3 : length(grid) ÷ 3 * 4
        
        newgrid = fill("", newsize)
        
        for y in 1:subsize:length(grid)
            for x in 1:subsize:length(grid)
                square = [grid[y+dy][x:x+subsize-1] for dy in 0:subsize-1]
                newsquare = enhance(join(square, "/"), rules)
                
                for (dy, row) in enumerate(split(newsquare, "/"))
                    newgrid[(y-1)÷subsize*((subsize+1))+dy] *= row
                end
            end
        end
        
        grid = newgrid
    end
    
    count = sum(c == '#' for row in grid for c in row)
    println(count)
end

function enhance(input::String, rules::Dict{String, String})
    for _ in 1:4
        haskey(rules, input) && return rules[input]
        input = rotate(input)
    end
    
    input = flip(input)
    
    for _ in 1:4
        haskey(rules, input) && return rules[input]
        input = rotate(input)
    end
    
    return ""
end

function rotate(input::String)
    parts = split(input, "/")
    size = length(parts)
    newparts = [join(part[x] for part in reverse(parts)) for x in 1:size]
    return join(newparts, "/")
end

function flip(input::String)
    return join(reverse(split(input, "/")), "/")
end

solve()
