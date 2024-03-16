open("input.txt", "r") do file
    count1 = 0
    count2 = 0
    for line in eachline(file)
        a, b = split(line, ",")
        a1, a2 = parse.(Int, split(a, "-"))
        b1, b2 = parse.(Int, split(b, "-"))
        if (a1 <= b1 && a2 >= b2) || (b1 <= a1 && b2 >= a2)
            count1 += 1
        end
        if (a1 <= b2 && a2 >= b1) || (b1 <= a2 && b2 >= a1)
            count2 += 1
        end
    end
    println("Part 1: $count1")
    println("Part 2: $count2")
end