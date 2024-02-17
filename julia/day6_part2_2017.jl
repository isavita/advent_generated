
function solve()
    # Step 1: Read Input
    input_data = readlines("input.txt")
    input_data = chomp(input_data[1])
    input_data = replace(input_data, '\t' => ' ')
    banks = [parse(Int, x) for x in split(input_data)]

    # Step 2: Initialize Variables
    seen = Dict()
    cycles = 0

    # Step 3: Redistribution Loop
    while true
        # Convert current banks state to string to store in map
        state = string(banks)

        # Step 4: Check for Repeats
        if haskey(seen, state)
            println("The size of the loop is: ", cycles - seen[state])
            return
        end
        seen[state] = cycles

        # Find the bank with most blocks
        max_index = findmax(banks)[2]

        # Perform redistribution
        blocks = banks[max_index]
        banks[max_index] = 0
        for i in 1:blocks
            banks[(max_index + i - 1) % length(banks) + 1] += 1
        end

        # Increment cycle counter
        cycles += 1
    end
end

solve()
