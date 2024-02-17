
data = read("input.txt", String)
lines = split(strip(data), '\n')
checksum = 0

for line in lines
    nums = split(line)
    minVal = parse(Int, nums[1])
    maxVal = parse(Int, nums[1])

    for numStr in nums
        num = parse(Int, numStr)
        if num < minVal
            minVal = num
        end
        if num > maxVal
            maxVal = num
        end
    end

    global checksum += (maxVal - minVal)  # Use global keyword to modify the global variable
end

println(checksum)
