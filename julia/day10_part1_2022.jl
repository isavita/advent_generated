
function calculate_signal_strengths(filename::String)
    # Read the input from the file
    instructions = readlines(filename)

    # Initialize variables
    X = 1
    cycle = 0
    signal_strengths = Dict{Int, Int}()
    target_cycles = [20, 60, 100, 140, 180, 220]

    # Process each instruction
    for instruction in instructions
        if startswith(instruction, "noop")
            cycle += 1
            # Check if the current cycle is one of the target cycles
            if cycle in target_cycles
                signal_strengths[cycle] = cycle * X
            end
        elseif startswith(instruction, "addx")
            # First cycle of addx
            cycle += 1
            if cycle in target_cycles
                signal_strengths[cycle] = cycle * X
            end
            
            # Second cycle of addx
            cycle += 1
            if cycle in target_cycles
                signal_strengths[cycle] = cycle * X
            end
            
            # Update X after the second cycle
            value = parse(Int, split(instruction)[2])
            X += value
        end
    end

    # Calculate the sum of the signal strengths
    total_signal_strength = sum(values(signal_strengths))
    return total_signal_strength
end

# Main function to execute the program
function main()
    filename = "input.txt"
    total_strength = calculate_signal_strengths(filename)
    println("The sum of the signal strengths is: $total_strength")
end

# Run the main function
main()
