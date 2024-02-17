
open("input.txt") do file
    calories_list = Int[]
    current_calories = 0

    for line in eachline(file)
        if isempty(line)
            push!(calories_list, current_calories)
            current_calories = 0
            continue
        end

        current_calories += parse(Int, line)
    end

    push!(calories_list, current_calories)
    sorted_calories_list = sort(calories_list, rev=true)

    top_three_sum = sum(sorted_calories_list[1:min(3, length(calories_list))])
    println(top_three_sum)
end
