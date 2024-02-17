
open("input.txt") do file
    total = 0
    for line in eachline(file)
        numbers = map(x -> parse(Int, x), split(line))
        for (i, num1) in enumerate(numbers)
            for (j, num2) in enumerate(numbers)
                if i != j && num1 % num2 == 0
                    total += num1 ÷ num2
                end
            end
        end
    end
    println(total)
end
