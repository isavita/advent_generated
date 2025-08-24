# Function to read and parse the input file
function parse_input(filename)
    connections = Dict{Int, Set{Int}}()
    open(filename, "r") do file
        for line in eachline(file)
            parts = split(line, " <-> ")
            program = parse(Int, parts[1])
            linked_programs = Set(parse.(Int, split(parts[2], ", ")))
            connections[program] = linked_programs
            for linked in linked_programs
                if haskey(connections, linked)
                    push!(connections[linked], program)
                else
                    connections[linked] = Set([program])
                end
            end
        end
    end
    return connections
end

# Function to find all programs in the same group as the starting program using BFS
function find_group(connections, start)
    visited = Set{Int}()
    queue = [start]
    while !isempty(queue)
        current = popfirst!(queue)
        if !in(current, visited)
            push!(visited, current)
            for neighbor in connections[current]
                if !in(neighbor, visited)
                    push!(queue, neighbor)
                end
            end
        end
    end
    return visited
end

# Function to count the total number of groups
function count_groups(connections)
    visited = Set{Int}()
    groups = 0
    for program in keys(connections)
        if !in(program, visited)
            group = find_group(connections, program)
            union!(visited, group)
            groups += 1
        end
    end
    return groups
end

# Main execution block
function main()
    connections = parse_input("input.txt")
    group_with_zero = find_group(connections, 0)
    println("Number of programs in the group containing program ID 0: ", length(group_with_zero))
    total_groups = count_groups(connections)
    println("Total number of groups: ", total_groups)
end

main()