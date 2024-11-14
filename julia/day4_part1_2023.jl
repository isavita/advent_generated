
function solve_scratchcards(filename)
    total_points = 0

    # Open the input file
    open(filename, "r") do file
        # Read the file line by line
        for line in eachline(file)
            # Split the line into card and numbers
            card_info = split(line, ":")
            number_lists = split(card_info[2], "|")

            # Parse winning numbers and your numbers
            winning_nums = Set(parse.(Int, split(strip(number_lists[1]))))
            your_nums = Set(parse.(Int, split(strip(number_lists[2]))))

            # Find matching numbers
            matches = intersect(winning_nums, your_nums)
            
            # Calculate points
            if length(matches) > 0
                points = 2^(length(matches) - 1)
                total_points += points
            end
        end
    end

    return total_points
end

# Read from input.txt and print the result
println(solve_scratchcards("input.txt"))
