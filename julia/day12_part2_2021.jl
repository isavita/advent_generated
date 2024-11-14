
function read_cave_connections(filename)
    connections = Dict{String, Vector{String}}()
    open(filename, "r") do file
        for line in eachline(file)
            cave1, cave2 = split(line, "-")
            push!(get!(connections, cave1, String[]), cave2)
            push!(get!(connections, cave2, String[]), cave1)
        end
    end
    return connections
end

function is_small_cave(cave)
    return islowercase(cave[1])
end

function count_paths(connections, part2::Bool=false)
    function dfs(current, visited, small_cave_twice)
        if current == "end"
            return 1
        end
        
        total_paths = 0
        for next_cave in connections[current]
            if next_cave == "start"
                continue
            end
            
            if !is_small_cave(next_cave)
                total_paths += dfs(next_cave, visited, small_cave_twice)
            else
                if next_cave ∉ visited
                    total_paths += dfs(next_cave, [visited; next_cave], small_cave_twice)
                elseif part2 && !small_cave_twice
                    total_paths += dfs(next_cave, visited, true)
                end
            end
        end
        
        return total_paths
    end
    
    return dfs("start", String[], false)
end

function solve_passage_pathing(filename)
    connections = read_cave_connections(filename)
    
    part1_result = count_paths(connections)
    part2_result = count_paths(connections, true)
    
    println("Part 1: ", part1_result)
    println("Part 2: ", part2_result)
end

solve_passage_pathing("input.txt")
