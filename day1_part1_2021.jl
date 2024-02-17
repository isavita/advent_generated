
using DelimitedFiles

data = readdlm("input.txt", Int)

global prev = 0
global count = 0

for current in data
    if prev != 0 && current > prev
        global count += 1
    end
    global prev = current
end

println(count)
