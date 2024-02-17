
using JSON

function sum_numbers(data)
    sum = 0
    if isa(data, Vector)
        for v in data
            sum += sum_numbers(v)
        end
    elseif isa(data, Dict)
        if !contains_red(data)
            for v in values(data)
                sum += sum_numbers(v)
            end
        end
    elseif isa(data, Number)
        sum += Int(data)
    end
    return sum
end

function contains_red(obj)
    for v in values(obj)
        if isa(v, String) && v == "red"
            return true
        end
    end
    return false
end

data = JSON.parsefile("input.txt")
sum = sum_numbers(data)
println(sum)
