
using DelimitedFiles

# Read input from the file
input = readdlm("input.txt", '\n', String)

# Initialize registers
registers = Dict{String, Int}()

# Process instructions
for line in input
    parts = split(line)
    reg, op, amount, _, cond_reg, cond_op, cond_val = parts
    amount = parse(Int, amount)
    cond_val = parse(Int, cond_val)

    # Check condition
    cond = false
    if cond_op == ">"
        cond = get(registers, cond_reg, 0) > cond_val
    elseif cond_op == ">="
        cond = get(registers, cond_reg, 0) >= cond_val
    elseif cond_op == "<"
        cond = get(registers, cond_reg, 0) < cond_val
    elseif cond_op == "<="
        cond = get(registers, cond_reg, 0) <= cond_val
    elseif cond_op == "=="
        cond = get(registers, cond_reg, 0) == cond_val
    elseif cond_op == "!="
        cond = get(registers, cond_reg, 0) != cond_val
    end

    if cond
        # Update register value
        if op == "inc"
            registers[reg] = get(registers, reg, 0) + amount
        elseif op == "dec"
            registers[reg] = get(registers, reg, 0) - amount
        end
    end
end

# Find max value
max_value = maximum(values(registers))

# Print the max value
println(max_value)
