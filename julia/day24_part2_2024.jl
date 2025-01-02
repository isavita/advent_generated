
struct Gate
    a::String
    op::String
    b::String
end

function parse_input(input::String)
    parts = split(input, "\n\n")
    gates = []
    for line in split(parts[2], "\n")
        if line == ""
            continue
        end
        parts = split(line, " -> ")
        gate_parts = split(parts[1], " ")
        push!(gates, (Gate(gate_parts[1], gate_parts[2], gate_parts[3]), parts[2]))
    end
    return gates
end

function create_lookups(gates)
    lookup = Dict{String, Gate}()
    reverse_lookup = Dict{String, String}()
    for (gate, output) in gates
        lookup[output] = gate
        inputs = sort([gate.a, gate.b])
        key = join([inputs[1], gate.op, inputs[2]], "_")
        reverse_lookup[key] = output
    end
    return lookup, reverse_lookup
end

function swap!(pairs, gates, a, b)
    push!(pairs, [a, b])
    for i in eachindex(gates)
        if gates[i][2] == a
            gates[i] = (gates[i][1], b)
        elseif gates[i][2] == b
            gates[i] = (gates[i][1], a)
        end
    end
end

function get_reverse_lookup_key(a, op, b)
    inputs = sort([a, b])
    return join([inputs[1], op, inputs[2]], "_")
end

function solution(gates)
    pairs = []
    num_z = count(g -> startswith(g[2], "z"), gates)
    
    for _ in 1:4
        adder = ""
        carry = ""
        lookup, reverse_lookup = create_lookups(gates)

        for i in 0:num_z-1
            xi = "x" * lpad(i, 2, "0")
            yi = "y" * lpad(i, 2, "0")
            zi = "z" * lpad(i, 2, "0")

            if i == 0
                adder = get(reverse_lookup, get_reverse_lookup_key(xi, "XOR", yi), "")
                carry = get(reverse_lookup, get_reverse_lookup_key(xi, "AND", yi), "")
            else
                bit = get(reverse_lookup, get_reverse_lookup_key(xi, "XOR", yi), "")
                if bit != ""
                    adder = get(reverse_lookup, get_reverse_lookup_key(bit, "XOR", carry), "")
                    if adder != ""
                        c1 = get(reverse_lookup, get_reverse_lookup_key(xi, "AND", yi), "")
                        c2 = get(reverse_lookup, get_reverse_lookup_key(bit, "AND", carry), "")
                        carry = get(reverse_lookup, get_reverse_lookup_key(c1, "OR", c2), "")
                    end
                end
            end

            if adder == ""
                gate = lookup[zi]
                bit_key = get_reverse_lookup_key(xi, "XOR", yi)
                bit = get(reverse_lookup, bit_key, "")
                if get(reverse_lookup, get_reverse_lookup_key(gate.a, "XOR", carry), "") != ""
                    swap!(pairs, gates, bit, gate.a)
                    break
                elseif get(reverse_lookup, get_reverse_lookup_key(gate.b, "XOR", carry), "") != ""
                    swap!(pairs, gates, bit, gate.b)
                    break
                end
            elseif adder != zi
                swap!(pairs, gates, adder, zi)
                break
            end
        end
    end

    result = reduce(vcat, pairs)
    sort!(result)
    return join(result, ",")
end

input = read("input.txt", String)
gates = parse_input(input)
println(solution(gates))
