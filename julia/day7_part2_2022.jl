
function solve_day7(filename)
    # Read input file
    lines = readlines(filename)
    
    # Dictionary to track directory sizes
    dir_sizes = Dict{String, Int}()
    
    # Stack to track current path
    current_path = String[]
    
    # Process each command and output
    for line in lines
        parts = split(line)
        
        # Change directory command
        if parts[1] == "\$" && parts[2] == "cd"
            if parts[3] == "/"
                current_path = ["/"]
            elseif parts[3] == ".."
                pop!(current_path)
            else
                push!(current_path, parts[3])
            end
        
        # File size information
        elseif tryparse(Int, parts[1]) !== nothing
            file_size = parse(Int, parts[1])
            
            # Update size for current and all parent directories
            for i in 1:length(current_path)
                path = join(current_path[1:i], "/")
                dir_sizes[path] = get(dir_sizes, path, 0) + file_size
            end
        end
    end
    
    # Part 1: Sum of directories with total size at most 100000
    part1 = sum(size for size in values(dir_sizes) if size <= 100000)
    
    # Part 2: Find smallest directory to delete
    total_space = 70000000
    needed_space = 30000000
    used_space = dir_sizes["/"]
    free_space = total_space - used_space
    
    part2 = minimum(size for size in values(dir_sizes) 
                    if free_space + size >= needed_space)
    
    return part1, part2
end

# Read input and solve
part1, part2 = solve_day7("input.txt")
println("Part 1: ", part1)
println("Part 2: ", part2)
