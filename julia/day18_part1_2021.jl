
import Base: +, show

mutable struct SnailNumber
    value::Int
    left::Union{SnailNumber, Nothing}
    right::Union{SnailNumber, Nothing}

    SnailNumber(value::Int) = new(value, nothing, nothing)
    SnailNumber(left::SnailNumber, right::SnailNumber) = new(-1, left, right) # Use -1 value to indicate a pair
end

is_regular(sn::SnailNumber) = sn.left === nothing && sn.right === nothing

function show(io::IO, sn::SnailNumber)
    if is_regular(sn)
        print(io, sn.value)
    else
        print(io, '[')
        show(io, sn.left)
        print(io, ',')
        show(io, sn.right)
        print(io, ']')
    end
end

function +(a::SnailNumber, b::SnailNumber)
    new_sn = SnailNumber(deepcopy(a), deepcopy(b)) # Deepcopy to avoid modifying originals during reduction
    reduce!(new_sn)
    return new_sn
end

function reduce!(sn::SnailNumber)
    while true
        exploded, _, _ = explode!(sn)
        if exploded
            continue
        end
        if !split!(sn)
            break
        end
    end
    return sn
end

function explode!(sn::SnailNumber, depth::Int = 0)
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

function add_left!(sn::SnailNumber, val::Int)
    if is_regular(sn)
        sn.value += val
    else
        add_left!(sn.left, val)
    end
end

function add_right!(sn::SnailNumber, val::Int)
    if is_regular(sn)
        sn.value += val
    else
        add_right!(sn.right, val)
    end
end

function split!(sn::SnailNumber)
    if is_regular(sn)
        if sn.value >= 10
            sn.left = SnailNumber(div(sn.value, 2))
            sn.right = SnailNumber(cld(sn.value, 2)) # ceil division
            sn.value = -1 # Mark as pair
            return true
        end
        return false
    end
    
    if split!(sn.left)
        return true
    end
    return split!(sn.right)
end

function magnitude(sn::SnailNumber)
    if is_regular(sn)
        return sn.value
    end
    return 3 * magnitude(sn.left) + 2 * magnitude(sn.right)
end

function parse_snail_number_recursive(s::AbstractString, index::Int)
    if isdigit(s[index])
        start_index = index
        val_str = ""
         while index <= length(s) && isdigit(s[index])
             val_str *= s[index]
             index += 1
         end
        val = parse(Int, val_str)
        return SnailNumber(val), index
    elseif s[index] == '['
        index += 1 # Skip '['
        left, index = parse_snail_number_recursive(s, index)
        # Expecting ','
        if s[index] != ',' error("Expected comma at index $index in $s") end
        index += 1 # Skip ','
        right, index = parse_snail_number_recursive(s, index)
        # Expecting ']'
        if s[index] != ']' error("Expected closing bracket at index $index in $s") end
        index += 1 # Skip ']'
        return SnailNumber(left, right), index
    else
        error("Invalid character at index $index in $s")
    end
end

function parse_snail_number(s::AbstractString)
    num, next_index = parse_snail_number_recursive(strip(s), 1)
    if next_index != length(strip(s)) + 1
         error("Did not consume entire string: $s, stopped at index $(next_index-1)")
    end
    return num
end

function main()
    snail_numbers = SnailNumber[]
    try
        open("input.txt", "r") do file
            for line in readlines(file)
                if !isempty(strip(line))
                    push!(snail_numbers, parse_snail_number(line))
                end
            end
        end
    catch e
        println("Error reading file input.txt: ", e)
        return
    end


    if isempty(snail_numbers)
        println("No snailfish numbers found in the file.")
    else
        result = snail_numbers[1]
        for i in 2:length(snail_numbers)
             # The + operator handles deepcopy and reduction
            result = result + snail_numbers[i]
        end
        println(magnitude(result))
    end
end

main()
