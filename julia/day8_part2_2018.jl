function read_input(filename::String)::Vector{Int}
    numbers = Int[]
    open(filename, "r") do file
        for num in split(readline(file))
            push!(numbers, parse(Int, num))
        end
    end
    return numbers
end

function parse_tree(data::Vector{Int}, index::Int)::Tuple{Int, Int}
    child_count, meta_count = data[index], data[index + 1]
    index += 2

    child_values = Int[]
    for i in 1:child_count
        child_value, index = parse_tree(data, index)
        push!(child_values, child_value)
    end

    value = 0
    if child_count == 0
        for i in 1:meta_count
            value += data[index + i - 1]
        end
    else
        for i in 1:meta_count
            metadata = data[index + i - 1]
            if 0 < metadata <= length(child_values)
                value += child_values[metadata]
            end
        end
    end
    index += meta_count

    return value, index
end

function main()
    numbers = read_input("input.txt")
    value, _ = parse_tree(numbers, 1)
    println(value)
end

main()