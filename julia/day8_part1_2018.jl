function read_input(filename::String)
    numbers = Int[]
    open(filename, "r") do file
        for num in split(readline(file))
            push!(numbers, parse(Int, num))
        end
    end
    return numbers
end

function parse_tree(data::Vector{Int}, index::Int)
    child_count, meta_count = data[index], data[index+1]
    index += 2

    sum = 0
    for i in 1:child_count
        child_sum, new_index = parse_tree(data, index)
        sum += child_sum
        index = new_index
    end

    for i in 1:meta_count
        sum += data[index+i-1]
    end
    index += meta_count

    return sum, index
end

numbers = read_input("input.txt")
sum, _ = parse_tree(numbers, 1)
println(sum)