
open("input.txt") do file
    maxCalories = 0
    currentCalories = 0

    for line in eachline(file)
        if line == ""
            if currentCalories > maxCalories
                maxCalories = currentCalories
            end
            currentCalories = 0
            continue
        end

        calories = parse(Int, line)
        currentCalories += calories
    end

    if currentCalories > maxCalories
        maxCalories = currentCalories
    end

    println(maxCalories)
end
