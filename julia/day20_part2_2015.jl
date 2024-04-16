function find_min_house_part1(target)
    house_presents = zeros(Int, target ÷ 10)
    for elf = 1:length(house_presents)
        for house = elf:elf:length(house_presents)
            house_presents[house] += 10 * elf
        end
    end
    for house = 1:length(house_presents)
        if house_presents[house] >= target
            return house
        end
    end
end

function find_min_house_part2(target)
    house_presents = zeros(Int, target ÷ 11)
    for elf = 1:length(house_presents)
        for house = elf:elf:min(length(house_presents), 50 * elf)
            house_presents[house] += 11 * elf
        end
    end
    for house = 1:length(house_presents)
        if house_presents[house] >= target
            return house
        end
    end
end

function main()
    # Read the target number of presents from input.txt
    target = parse(Int, readchomp(open("input.txt")))
    
    # Part 1: Find the lowest house number for the first challenge
    house_number_part1 = find_min_house_part1(target)
    println("Lowest house number (Part 1): ", house_number_part1)
    
    # Part 2: Find the lowest house number for the second challenge
    house_number_part2 = find_min_house_part2(target)
    println("Lowest house number (Part 2): ", house_number_part2)
end

main()