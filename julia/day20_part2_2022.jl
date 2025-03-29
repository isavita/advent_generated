
using DataStructures

function read_input(filename="input.txt")
    lines = readlines(filename)
    return [parse(Int, line) for line in lines if !isempty(strip(line))]
end

function mix!(current_order::Vector{Int}, values::Dict{Int, Int})
    n = length(current_order)
    len_minus_1 = n - 1
    if len_minus_1 <= 0
        return
    end

    original_indices = 1:n

    for original_idx in original_indices
        value_to_move = values[original_idx]
        if value_to_move == 0
            continue
        end

        old_pos_idx = findfirst(==(original_idx), current_order)

        # Calculate target index (1-based) after movement
        # The move is relative to the list of size n-1 (excluding the item itself)
        target_pos_idx = mod(old_pos_idx - 1 + value_to_move, len_minus_1) + 1

        # Move the element
        item_to_move = current_order[old_pos_idx]
        deleteat!(current_order, old_pos_idx)
        insert!(current_order, target_pos_idx, item_to_move)
    end
end

function coords(current_order::Vector{Int}, values::Dict{Int, Int})
    n = length(current_order)
    zero_original_idx = -1
    for (orig_idx, val) in values
        if val == 0
            zero_original_idx = orig_idx
            break
        end
    end

    zero_pos_idx = findfirst(==(zero_original_idx), current_order)

    idx1000 = mod1(zero_pos_idx + 1000, n)
    idx2000 = mod1(zero_pos_idx + 2000, n)
    idx3000 = mod1(zero_pos_idx + 3000, n)

    val1000 = values[current_order[idx1000]]
    val2000 = values[current_order[idx2000]]
    val3000 = values[current_order[idx3000]]

    return val1000 + val2000 + val3000
end

function main()
    input_values = read_input("input.txt")
    n = length(input_values)

    key = 811589153
    values = Dict{Int, Int}(i => Int(val) * key for (i, val) in enumerate(input_values))
    current_order = collect(1:n)

    for _ in 1:10
        mix!(current_order, values)
    end

    result = coords(current_order, values)
    println(result)
end

main()
