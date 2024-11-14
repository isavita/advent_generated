
function solve_scratchcards(filename)
    # Part 1: Calculate total points
    total_points = 0
    
    # Part 2: Track number of card instances
    card_instances = ones(Int, countlines(filename))
    
    # Read input file
    open(filename, "r") do file
        for (card_index, line) in enumerate(eachline(file))
            # Parse card numbers
            card_parts = split(line, ": ")[2]
            winning_nums, my_nums = map(x -> parse.(Int, split(x)), split(card_parts, " | "))
            
            # Find matching numbers
            matches = intersect(winning_nums, my_nums)
            num_matches = length(matches)
            
            # Part 1: Calculate points
            total_points += num_matches > 0 ? 2^(num_matches - 1) : 0
            
            # Part 2: Update card instances
            for i in (card_index + 1):(card_index + num_matches)
                card_instances[i] += card_instances[card_index]
            end
        end
    end
    
    # Return results
    return (total_points, sum(card_instances))
end

# Read from input.txt and print results
function main()
    points, total_cards = solve_scratchcards("input.txt")
    println("Part 1 - Total Points: ", points)
    println("Part 2 - Total Scratchcards: ", total_cards)
end

main()
