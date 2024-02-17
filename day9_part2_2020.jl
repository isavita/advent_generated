
numbers = open("input.txt") do file
    [parse(Int, x) for x in readlines(file)]
end

invalidNumber = 14360655

for i in 1:length(numbers)
    sum = numbers[i]
    min = numbers[i]
    max = numbers[i]
    for j in i+1:length(numbers)
        sum += numbers[j]
        if numbers[j] < min
            min = numbers[j]
        end
        if numbers[j] > max
            max = numbers[j]
        end
        if sum == invalidNumber
            println(min + max)
            return
        elseif sum > invalidNumber
            break
        end
    end
end
