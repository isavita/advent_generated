
import Base: +, values

struct File
    size::Int
end

mutable struct Directory
    files::Dict{String, File}
    directories::Dict{String, Directory}

    Directory() = new(Dict{String, File}(), Dict{String, Directory}())
end

# Calculates total size and the sum of sizes of directories <= 100000 in one pass
function calculate_sizes(dir::Directory)::Tuple{Int, Int}
    total_dir_size = 0
    sum_small_dirs = 0

    for f in values(dir.files)
        total_dir_size += f.size
    end

    for d in values(dir.directories)
        sub_total, sub_sum_small = calculate_sizes(d)
        total_dir_size += sub_total
        sum_small_dirs += sub_sum_small
    end

    if total_dir_size <= 100000
        sum_small_dirs += total_dir_size
    end

    return (total_dir_size, sum_small_dirs)
end


function main()
    root = Directory()
    current_dir = root
    directory_stack = Directory[root] # Explicitly type the stack

    open("input.txt", "r") do file
        for line in eachline(file)
            parts = split(strip(line))

            if parts[1] == "\$"
                if parts[2] == "cd"
                    path = parts[3]
                    if path == "/"
                        current_dir = root
                        directory_stack = [root]
                    elseif path == ".."
                        if length(directory_stack) > 1
                            pop!(directory_stack)
                            current_dir = directory_stack[end]
                        end
                    else
                        # Ensure directory exists (ls might not have run yet)
                        if !haskey(current_dir.directories, path)
                             current_dir.directories[path] = Directory()
                        end
                        current_dir = current_dir.directories[path]
                        push!(directory_stack, current_dir)
                    end
                # We can ignore "$ ls" command itself
                end
            elseif parts[1] == "dir"
                dir_name = parts[2]
                # Ensure directory doesn't already exist from a prior 'cd'
                 if !haskey(current_dir.directories, dir_name)
                     current_dir.directories[dir_name] = Directory()
                 end
            else # Must be a file listing
                 # Attempt to parse size, ignore if fails (like header lines, though unlikely here)
                 try
                    size = parse(Int, parts[1])
                    name = parts[2]
                    current_dir.files[name] = File(size)
                 catch e
                    # Ignore lines that cannot be parsed as "size name"
                    if !(e isa ArgumentError)
                        rethrow(e) # Rethrow unexpected errors
                    end
                 end
            end
        end
    end

    # Calculate total sizes and the sum of small directories simultaneously
    _, final_sum_sizes = calculate_sizes(root)

    println(final_sum_sizes)
end

main()
