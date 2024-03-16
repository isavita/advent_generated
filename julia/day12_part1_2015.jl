using JSON

data = open(f -> read(f, String), "input.txt") |> JSON.parse

function sum_numbers(data)
    if isa(data, Number)
        return data
    elseif isa(data, Dict)
        return sum(sum_numbers(value) for (key, value) in data)
    elseif isa(data, Array)
        return sum(sum_numbers(element) for element in data)
    else
        return 0
    end
end

result = sum_numbers(data)
println(result)