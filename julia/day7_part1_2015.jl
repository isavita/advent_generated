# Define a function to evaluate an expression given the current state of the circuit
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


# Function to process instructions and simulate the circuit
function simulate_circuit(filename)
    circuit = Dict{String, Int}()
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

# Simulate the circuit and print the signal value for wire a
circuit = simulate_circuit("input.txt")
println(circuit["a"])
