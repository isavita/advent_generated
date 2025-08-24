# Function to perform the look-and-say process
function look_and_say(sequence::String)
    result = []
    count = 1
    for i in 2:length(sequence)
        if sequence[i] == sequence[i-1]
            count += 1
        else
            push!(result, string(count, sequence[i-1]))
            count = 1
        end
    end
    push!(result, string(count, sequence[end]))
    return join(result)
end

# Function to apply look-and-say n times
function apply_look_and_say(sequence::String, n::Int)
    for _ in 1:n
        sequence = look_and_say(sequence)
    end
    return sequence
end

# Read the initial sequence from input.txt
function read_input(filename::String)
    open(filename, "r") do file
        return String(strip(readline(file)))
    end
end

# Main function to run the challenge
function main()
    initial_sequence = read_input("input.txt")

    # Part 1: Apply the process 40 times
    result_after_40 = apply_look_and_say(initial_sequence, 40)
    println("Length after 40 iterations: ", length(result_after_40))

    # Part 2: Apply the process 50 times
    result_after_50 = apply_look_and_say(initial_sequence, 50)
    println("Length after 50 iterations: ", length(result_after_50))
end

# Execute the main function
main()