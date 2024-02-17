
open("input.txt") do file
    vals = [parse(Int, line) for line in eachline(file) if line != ""]
    
    prevSum = vals[1] + vals[2] + vals[3]
    count = 0
    for i = 4:length(vals)
        currSum = vals[i-2] + vals[i-1] + vals[i]
        if currSum > prevSum
            count += 1
        end
        prevSum = currSum
    end
    
    println(count)
end
