function validatePassword(policy::SubString{String}, password::SubString{String})
    parts = split(policy, ['-', ' '])
    min, max, char = parse(Int, parts[1]), parse(Int, parts[2]), parts[3][1]
    return (password[min] == char) ⊻ (password[max] == char)
end

function main()
    validCount = 0
    open("input.txt") do file
        for line in eachline(file)
            if ':' in line
                parts = split(line, ':')
                policy, password = parts[1], strip(parts[2])
                if validatePassword(policy, password)
                    validCount += 1
                end
            end
        end
    end
    println(validCount)
end

main()