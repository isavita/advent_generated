
function calculate_difference(filename::String)
    total_code_length = 0
    total_memory_length = 0

    open(filename, "r") do file
        for line in eachline(file)
            # Remove the surrounding quotes
            code_length = length(line)
            memory_string = Meta.parse(line)  # Parse the string to unescape it
            memory_length = length(memory_string)

            total_code_length += code_length
            total_memory_length += memory_length
        end
    end

    return total_code_length - total_memory_length
end

# Read from input.txt and print the result
result = calculate_difference("input.txt")
println(result)
