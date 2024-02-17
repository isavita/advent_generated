
expenses = open("input.txt") do file
    [parse(Int, line) for line in eachline(file)]
end

for i in 1:length(expenses)
    for j in i+1:length(expenses)
        for k in j+1:length(expenses)
            if expenses[i] + expenses[j] + expenses[k] == 2020
                println(expenses[i] * expenses[j] * expenses[k])
                return
            end
        end
    end
end
