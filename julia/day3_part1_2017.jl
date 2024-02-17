
function main()
    # Step 1: Read input
    input_file = open("input.txt")
    target = parse(Int, chomp(readline(input_file)))
    close(input_file)

    # Step 2: Find the square
    side_length = ceil(Int, sqrt(target))
    if iseven(side_length)
        side_length += 1
    end

    # Step 3: Find distance to the nearest middle point
    max_value = side_length * side_length
    steps_from_edge = (side_length - 1) ÷ 2
    distance_to_middle = typemax(Int)

    for i in 0:3
        middle_point = max_value - steps_from_edge - (side_length - 1) * i
        distance = abs(target - middle_point)
        if distance < distance_to_middle || i == 0
            distance_to_middle = distance
        end
    end

    # Step 4: Calculate Manhattan Distance
    manhattan_distance = steps_from_edge + distance_to_middle

    println(manhattan_distance)
end

main()
