
function main()
    counts = zeros(Int, 12, 2)
    open("input.txt") do file
        for line in eachline(file)
            for (i, digit) in enumerate(line)
                counts[i, digit - '0' + 1] += 1
            end
        end
    end

    gamma_rate = 0
    epsilon_rate = 0
    for i in 1:12
        if counts[i, 1] > counts[i, 2]
            gamma_rate |= 1 << (12 - i)
        else
            epsilon_rate |= 1 << (12 - i)
        end
    end

    println(gamma_rate * epsilon_rate)
end

main()
