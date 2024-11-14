
function find_first_and_last_digit(line)
    digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    first_digit = last_digit = nothing

    for i in 1:length(line)
        char = line[i]
        if '0' <= char <= '9'
            digit = char - '0'
            if first_digit === nothing
                first_digit = digit
            end
            last_digit = digit
        else
            for j in 1:10
                if startswith(line[i:end], digits[j])
                    if first_digit === nothing
                        first_digit = j - 1
                    end
                    last_digit = j - 1
                    break
                end
            end
        end
    end

    return first_digit, last_digit
end

function main()
    sum = 0
    open("input.txt", "r") do file
        for line in eachline(file)
            first_digit, last_digit = find_first_and_last_digit(line)
            sum += 10 * first_digit + last_digit
        end
    end
    println(sum)
end

main()
