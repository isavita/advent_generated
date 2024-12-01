
function solve_part1(left, right)
    left_sorted = sort(left)
    right_sorted = sort(right)
    total_distance = 0
    for i in 1:length(left_sorted)
        total_distance += abs(left_sorted[i] - right_sorted[i])
    end
    return total_distance
end

function solve_part2(left, right)
    right_counts = Dict()
    for num in right
        right_counts[num] = get(right_counts, num, 0) + 1
    end
    total_similarity = 0
    for num in left
        total_similarity += num * get(right_counts, num, 0)
    end
    return total_similarity
end


function main()
    try
        data = readlines("input.txt")
        left_list = []
        right_list = []
        for line in data
            l, r = split(line)
            push!(left_list, parse(Int, l))
            push!(right_list, parse(Int, r))
        end

        part1_result = solve_part1(left_list, right_list)
        println("Part 1: ", part1_result)

        part2_result = solve_part2(left_list, right_list)
        println("Part 2: ", part2_result)

    catch e
        println("Error: ", e)
    end
end

main()

