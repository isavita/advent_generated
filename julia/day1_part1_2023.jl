open("input.txt", "r") do file
    sum = 0
    for line in eachline(file)
        if !isempty(line)
            first_digit = -1
            last_digit = -1
            for char in line
                if isdigit(char)
                    if first_digit == -1
                        first_digit = parse(Int, char)
                    end
                    last_digit = parse(Int, char)
                end
            end
            if first_digit != -1 && last_digit != -1
                value = first_digit * 10 + last_digit
                sum += value
            end
        end
    end
    println(sum)
end