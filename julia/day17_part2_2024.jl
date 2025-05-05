
import Base: ==, hash
import Base: getindex # For tuple access in check

mutable struct Program
    a::Int64
    b::Int64
    c::Int64
    program::Vector{Int}
end

function compute_operand(val::Int64, a::Int64, b::Int64, c::Int64)::Int64
    if val == 0 || val == 1 || val == 2 || val == 3
        return val
    elseif val == 4
        return a
    elseif val == 5
        return b
    elseif val == 6
        return c
    else
        error("Invalid combo operand: $val")
    end
end

function simulate_computer(p::Program)::Vector{Int}
    outs = Vector{Int}()
    a, b, c = p.a, p.b, p.c
    program = p.program

    idx = 1 # 1-based index pointing to the opcode
    while idx <= length(program)
        opcode = program[idx]
        operand = program[idx+1]

        next_idx = idx + 2 # Default next instruction

        if opcode == 0
            shift_amount = compute_operand(Int64(operand), a, b, c)
            a >>= shift_amount
        elseif opcode == 1
            b ⊻= Int64(operand)
        elseif opcode == 2
            b = compute_operand(Int64(operand), a, b, c) % 8
        elseif opcode == 3
            if a != 0
                # Jump: operand is 1-based index of target opcode
                target_opcode_idx = operand
                next_idx = target_opcode_idx + 1 # Next idx will point to target operand + 1
            end
        elseif opcode == 4
            b ⊻= c
        elseif opcode == 5
            # Output and stop simulation
            push!(outs, Int(compute_operand(Int64(operand), a, b, c) % 8))
            return outs # Stop after the first output
        elseif opcode == 6
            shift_amount = compute_operand(Int64(operand), a, b, c)
            b = a >> shift_amount
        elseif opcode == 7
            shift_amount = compute_operand(Int64(operand), a, b, c)
            c = a >> shift_amount
        else
            error("Invalid opcode: $opcode")
        end

        idx = next_idx
    end
    return outs # Return empty if no output was produced
end

# Define state as a Tuple{Int, Int64} for stack and seen set
# (depth, score)

function check(p::Program)::Vector{Int64}
    program = p.program
    valids = Vector{Int64}()
    stack = Vector{Tuple{Int, Int64}}()
    push!(stack, (0, Int64(0)))
    seen = Set{Tuple{Int, Int64}}()

    while !isempty(stack)
        state = pop!(stack)
        depth, score = state

        if state in seen
            continue
        end
        push!(seen, state)

        if depth == length(program)
            push!(valids, score)
        else
            for i in Int64(0):7
                # Build score from LSD (depth 0) to MSD (depth len(program)-1)
                # The digit 'i' is added at position 'depth' (0-indexed from right)
                new_score = i + 8 * score

                # Create a test program with the potential initial 'a' value
                test_program = Program(new_score, p.b, p.c, program)

                # Simulate and get the first output
                result = simulate_computer(test_program)

                # Check if simulation produced output and if the first output matches the target
                target_output = program[length(program) - depth] % 8 # Target is program[end-depth] % 8
                if !isempty(result) && result[1] == target_output
                    # If it matches, explore this path further
                    push!(stack, (depth + 1, new_score))
                end
            end
        end
    end
    return valids
end

function main()
    a, b, c = Int64(0), Int64(0), Int64(0)
    program_vec = Vector{Int}()

    open("input.txt", "r") do f
        for line in readlines(f)
            line = strip(line)
            if startswith(line, "Register A:")
                a = parse(Int64, strip(split(line, ":")[2]))
            elseif startswith(line, "Register B:")
                b = parse(Int64, strip(split(line, ":")[2]))
            elseif startswith(line, "Register C:")
                c = parse(Int64, strip(split(line, ":")[2]))
            elseif startswith(line, "Program:")
                nums_str = split(strip(split(line, ":")[2]), ",")
                program_vec = [parse(Int, strip(n)) for n in nums_str]
            end
        end
    end

    p = Program(a, b, c, program_vec)

    # Part 2: Find the minimum valid value for initial 'a'
    valid_values = check(p)

    if isempty(valid_values)
        println("No valid values found.")
    else
        min_val = minimum(valid_values)
        println(min_val)
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
