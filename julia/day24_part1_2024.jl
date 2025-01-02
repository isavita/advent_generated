
function solve()
    lines = readlines("input.txt")
    wires = Dict{String, Int}()
    gates = []
    
    i = 1
    while i <= length(lines)
        line = strip(lines[i])
        if isempty(line)
            i += 1
            break
        end
        m = match(r"^(\w+):\s*([01])$", line)
        if m !== nothing
            wires[m[1]] = parse(Int, m[2])
        else
            println("Invalid wire definition: ", line)
            return
        end
        i += 1
    end

    while i <= length(lines)
        line = strip(lines[i])
        if isempty(line)
            i += 1
            continue
        end
        m = match(r"^(\w+)\s+(AND|OR|XOR)\s+(\w+)\s+->\s+(\w+)$", line)
        if m !== nothing
            push!(gates, (m[1], m[2], m[3], m[4]))
        else
            println("Invalid gate definition: ", line)
            return
        end
        i += 1
    end

    while !isempty(gates)
        progress = false
        new_gates = []
        for (in1, op, in2, out) in gates
            if haskey(wires, in1) && haskey(wires, in2)
                val1 = wires[in1]
                val2 = wires[in2]
                output_val = if op == "AND"
                    val1 & val2
                elseif op == "OR"
                    val1 | val2
                elseif op == "XOR"
                    val1 ⊻ val2
                else
                    println("Unknown operation: ", op)
                    return
                end
                wires[out] = output_val
                progress = true
            else
                push!(new_gates, (in1, op, in2, out))
            end
        end
        if !progress
            println("Cannot evaluate remaining gates due to missing inputs or cyclic dependencies.")
            return
        end
        gates = new_gates
    end

    z_wires = Dict{Int, Int}()
    for (wire, val) in wires
        m = match(r"^z(\d+)$", wire)
        if m !== nothing
            z_wires[parse(Int, m[1])] = val
        end
    end

    if isempty(z_wires)
        println("No wires starting with 'z' found.")
        return
    end

    sorted_indices = sort(collect(keys(z_wires)), rev=true)
    binary_string = ""
    for idx in sorted_indices
        binary_string *= string(z_wires[idx])
    end

    println(parse(Int, binary_string, base=2))
end

solve()
