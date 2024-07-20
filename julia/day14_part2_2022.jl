
using Printf

function parse_input(filename)
    paths = []
    open(filename, "r") do file
        for line in eachline(file)
            path = [parse.(Int, split(point, ",")) for point in split(line, " -> ")]
            push!(paths, path)
        end
    end
    return paths
end

function create_cave(paths)
    cave = Dict{Tuple{Int, Int}, Char}()
    for path in paths
        for i in 1:length(path)-1
            x1, y1 = path[i]
            x2, y2 = path[i+1]
            if x1 == x2
                for y in min(y1, y2):max(y1, y2)
                    cave[(x1, y)] = '#'
                end
            else
                for x in min(x1, x2):max(x1, x2)
                    cave[(x, y1)] = '#'
                end
            end
        end
    end
    return cave
end

function simulate_sand(cave, floor_y)
    sand_count = 0
    source = (500, 0)
    
    while true
        x, y = source
        if (x, y) in keys(cave) || y >= floor_y
            break
        end
        
        while true
            if !((x, y + 1) in keys(cave)) && y + 1 < floor_y
                y += 1
            elseif !((x - 1, y + 1) in keys(cave)) && y + 1 < floor_y
                x -= 1
                y += 1
            elseif !((x + 1, y + 1) in keys(cave)) && y + 1 < floor_y
                x += 1
                y += 1
            else
                break
            end
        end
        
        if y < floor_y
            cave[(x, y)] = 'o'
            sand_count += 1
        else
            break
        end
    end
    
    return sand_count
end

function main()
    paths = parse_input("input.txt")
    cave = create_cave(paths)
    
    # Determine the floor level
    floor_y = maximum(y for (_, y) in keys(cave)) + 2
    
    # Simulate sand falling until the source is blocked
    sand_count = simulate_sand(cave, floor_y)
    
    println("Units of sand that come to rest: $sand_count")
end

main()
