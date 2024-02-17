function find_basement(input)
    floor = 0
    position = 1
    for char in input
        if char == '('
            floor += 1
        elseif char == ')'
            floor -= 1
        end
        if floor < -1
            return position
        end
        position += 1
    end
    return nothing
end

function main()
    input = readlines("input.txt")[1]
    result = find_basement(input)
    println(result)
end

main()
