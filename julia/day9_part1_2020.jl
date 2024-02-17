
function is_valid(number, previous_numbers)
    seen = Dict()
    for n in previous_numbers
        if haskey(seen, number - n)
            return true
        end
        seen[n] = true
    end
    return false
end

preamble_length = 25
numbers = parse.(Int, readlines("input.txt"))

for i in preamble_length+1:length(numbers)
    if !is_valid(numbers[i], numbers[i-preamble_length:i-1])
        println(numbers[i])
        break
    end
end
