data = readlines("input.txt")[1]
result = sum([parse(Int, data[i]) for i in 1:length(data) if data[i] == data[mod1(i+1, length(data))]])
println(result)