
numbers = open("input.txt") do file
    [parse(Int, line) for line in eachline(file) if line != ""]
end

for i in 1:length(numbers)-1
    for j in i+1:length(numbers)
        if numbers[i] + numbers[j] == 2020
            println(numbers[i] * numbers[j])
            return
        end
    end
end
