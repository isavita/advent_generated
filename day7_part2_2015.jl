function evaluate(expr, circuit)
    if occursin(r"^[a-z]+$", expr) # If the expression is a wire identifier
        if haskey(circuit, expr)
            return circuit[expr] # Return the signal from the circuit if available
        else
            return -1 # Signal not yet available
        end
    elseif occursin(r"^\d+$", expr) # If the expression is a numeric literal
        return parse(Int, expr) # Directly return the numeric value
    else
        return -1 # For operations or unresolved wires, return -1
    end
end

# Helper function to perform the bitwise operations
function switch(op, left, right)
    if op == "AND"
        return left & right
    elseif op == "OR"
        return left | right
    elseif op == "LSHIFT"
        return (left << right) & 0xFFFF
    elseif op == "RSHIFT"
        return left >> right
    end
end

function simulate_circuit(filename, initial_conditions = Dict{String, Int}())
    circuit = Dict{String, Int}()
    apply_initial_conditions!(circuit, initial_conditions)
    instructions = readlines(filename)
    while length(instructions) > 0
        for i in length(instructions):-1:1
            parts = split(instructions[i])
            if parts[2] == "->"
                signal = evaluate(parts[1], circuit)
                if signal != -1
                    circuit[parts[3]] = signal
                    deleteat!(instructions, i)
                end
            elseif parts[1] == "NOT"
                signal = evaluate(parts[2], circuit)
                if signal != -1
                    circuit[parts[4]] = ~signal & 0xFFFF
                    deleteat!(instructions, i)
                end
            else
                op = parts[2]
                left = evaluate(parts[1], circuit)
                right = evaluate(parts[3], circuit)
                if left != -1 && right != -1
                    result = switch(op, left, right)
                    circuit[parts[5]] = result
                    deleteat!(instructions, i)
                end
            end
        end
    end
    return circuit
end

# Apply initial conditions to the circuit
function apply_initial_conditions!(circuit, conditions)
    for (wire, signal) in conditions
        circuit[wire] = signal
    end
end

# Part One: Find the initial signal for wire a
initial_circuit = simulate_circuit("input.txt")
initial_signal_a = initial_circuit["a"]

# Part Two: Override wire b, reset the circuit, and find the new signal for wire a
initial_conditions = Dict("b" => initial_signal_a)
new_circuit = simulate_circuit("input.txt", initial_conditions)
new_signal_a = new_circuit["a"]

println(new_signal_a)

