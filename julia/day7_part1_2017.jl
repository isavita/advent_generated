function find_bottom_program(filename::String)
    supports = Dict{String, Vector{String}}()
    is_supported = Set{String}()

    # Read the file and build the relationships
    open(filename, "r") do file
        for line in eachline(file)
            parts = split(line, " -> ")
            program = split(parts[1])[1]  # Get the program name from the first part
            if length(parts) > 1
                children = split(parts[2], ", ")
                supports[program] = children
                union!(is_supported, children)
            end
        end
    end

    # Find the bottom program
    for program in keys(supports)
        if !(program in is_supported)
            return program
        end
    end

    return nothing  # In case no bottom is found, which should not happen in valid inputs
end

# Main execution block
const INPUT_FILE = "input.txt"
bottom_program = find_bottom_program(INPUT_FILE)
println("The bottom program is: ", bottom_program)