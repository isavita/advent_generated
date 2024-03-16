# Read input from file
input = readlines("input.txt")

# Initialize variables
global x = 1
global cycle = 1
signal_strengths = []

# Iterate through the instructions
for instruction in input
    if instruction == "noop"
        # Execute noop instruction
        global cycle += 1
        if (cycle - 20) % 40 == 0
            push!(signal_strengths, cycle * x)
        end
    else
        # Execute addx instruction
        global cycle += 1
        if (cycle - 20) % 40 == 0
            push!(signal_strengths, cycle * x)
        end
        global cycle += 1
        if (cycle - 20) % 40 == 0
            push!(signal_strengths, cycle * x)
        end
        global x += parse(Int, split(instruction, " ")[2])
    end
end

# Print the sum of the signal strengths
println(sum(signal_strengths))

# Initialize variables for part 2
global x = 1
global cycle = 1
crt = []

# Iterate through the instructions again
for instruction in input
    if instruction == "noop"
        # Execute noop instruction
        if abs(((cycle - 1) % 40) - x) <= 1
            push!(crt, "#")
        else
            push!(crt, ".")
        end
        global cycle += 1
    else
        # Execute addx instruction
        if abs(((cycle - 1) % 40) - x) <= 1
            push!(crt, "#")
        else
            push!(crt, ".")
        end
        global cycle += 1
        if abs(((cycle - 1) % 40) - x) <= 1
            push!(crt, "#")
        else
            push!(crt, ".")
        end
        global cycle += 1
        global x += parse(Int, split(instruction, " ")[2])
    end
end

# Print the CRT image
for i in 1:6
    println(join(crt[(i-1)*40+1:i*40]))
end