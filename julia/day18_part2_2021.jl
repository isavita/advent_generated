
import Base: +

mutable struct SnailNumber
    value::Union{Int, Nothing}
    left::Union{SnailNumber, Nothing}
    right::Union{SnailNumber, Nothing}
end

SnailNumber(value::Int) = SnailNumber(value, nothing, nothing)
SnailNumber(left::SnailNumber, right::SnailNumber) = SnailNumber(nothing, left, right)

is_regular(sn::SnailNumber) = sn.left === nothing && sn.right === nothing

function deep_copy(sn::SnailNumber)
    if is_regular(sn)
        return SnailNumber(sn.value)
    else
        return SnailNumber(deep_copy(sn.left), deep_copy(sn.right))
    end
end

function add_left!(sn::SnailNumber, value::Int)
    if is_regular(sn)
        sn.value += value
    else
        add_left!(sn.left, value)
    end
end

function add_right!(sn::SnailNumber, value::Int)
    if is_regular(sn)
        sn.value += value
    else
        add_right!(sn.right, value)
    end
end

function explode!(sn::SnailNumber, depth::Int)
    if is_regular(sn)
        return false, 0, 0
    end

    if depth == 4
        left_val = sn.left.value
        right_val = sn.right.value
        sn.value = 0
        sn.left = nothing
        sn.right = nothing
        return true, left_val, right_val
    end

    exploded, left_val, right_val = explode!(sn.left, depth + 1)
    if exploded
        if right_val > 0 && sn.right !== nothing
            add_left!(sn.right, right_val)
        end
        return true, left_val, 0
    end

    exploded, left_val, right_val = explode!(sn.right, depth + 1)
    if exploded
        if left_val > 0 && sn.left !== nothing
            add_right!(sn.left, left_val)
        end
        return true, 0, right_val
    end

    return false, 0, 0
end

function split!(sn::SnailNumber)
    if is_regular(sn)
        if sn.value >= 10
            sn.left = SnailNumber(sn.value ÷ 2)
            sn.right = SnailNumber((sn.value + 1) ÷ 2) # Equivalent to ceil(sn.value / 2)
            sn.value = nothing
            return true
        end
        return false
    else
        return split!(sn.left) || split!(sn.right)
    end
end

function reduce!(sn::SnailNumber)
    while true
        exploded, _, _ = explode!(sn, 0)
        if exploded
            continue
        end
        if !split!(sn)
            break
        end
    end
    return sn
end

function +(a::SnailNumber, b::SnailNumber)
    new_number = SnailNumber(a, b) # Note: Does not copy a and b initially
    return reduce!(new_number)     # Reduce modifies the new structure
end

function magnitude(sn::SnailNumber)
    if is_regular(sn)
        return sn.value
    else
        return 3 * magnitude(sn.left) + 2 * magnitude(sn.right)
    end
end

function parse_snail_number(s::AbstractString)
    s = strip(s)
    if s[1] != '['
        return SnailNumber(parse(Int, s))
    end

    balance = 0
    split_idx = 0
    # Iterate through character indices within the outer brackets
    for i in 2:length(s)-1
        char = s[i]
        if char == '['
            balance += 1
        elseif char == ']'
            balance -= 1
        elseif char == ',' && balance == 0
            split_idx = i
            break
        end
    end

    left_str = s[2:split_idx-1]
    right_str = s[split_idx+1:end-1]
    left_sn = parse_snail_number(left_str)
    right_sn = parse_snail_number(right_str)
    return SnailNumber(left_sn, right_sn)
end


function main()
    lines = readlines("input.txt")
    snail_numbers = [parse_snail_number(line) for line in lines]

    if isempty(snail_numbers)
        println("0") # Or handle error as appropriate
        return
    end

    largest_magnitude = 0
    n = length(snail_numbers)
    for i in 1:n
        for j in 1:n
            if i == j
                continue
            end
            # Must deep copy before adding as reduction modifies the number
            sum1 = magnitude(deep_copy(snail_numbers[i]) + deep_copy(snail_numbers[j]))
            sum2 = magnitude(deep_copy(snail_numbers[j]) + deep_copy(snail_numbers[i]))
            largest_magnitude = max(largest_magnitude, sum1, sum2)
        end
    end

    println(largest_magnitude)
end

main()
