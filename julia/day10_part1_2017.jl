
data = open("input.txt") do file
    split(readline(file), ",")
end

lengths = parse.(Int, data)

list = collect(0:255)
global currentPosition = 0
global skipSize = 0

for length in lengths
    for i in 1:length÷2
        start = (currentPosition + i - 1) % 256 + 1
        stop = (currentPosition + length - i) % 256 + 1
        list[start], list[stop] = list[stop], list[start]
    end
    global currentPosition = (currentPosition + length + skipSize) % 256
    global skipSize += 1
end

result = list[1] * list[2]
println(result)
