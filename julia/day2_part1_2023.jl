
# Function to check if a game is possible with the given cube limits
function is_possible(game_data, limits)
    for subset in game_data
        for (color, count) in subset
            if count > limits[color]
                return false
            end
        end
    end
    return true
end

# Function to parse the input data from the file
function parse_input(filename)
    games = []
    open(filename, "r") do file
        for line in eachline(file)
            parts = split(line, ": ")
            game_id = parse(Int, parts[1][5:end])  # Extract the game ID
            subsets = split(parts[2], "; ")
            game_data = []
            for subset in subsets
                color_counts = Dict{Symbol, Int}()
                for color_info in split(subset, ", ")
                    count, color = split(color_info, " ")
                    color_counts[Symbol(color)] = parse(Int, count)
                end
                push!(game_data, color_counts)
            end
            push!(games, (game_id, game_data))
        end
    end
    return games
end

# Main function to calculate the sum of possible game IDs
function main()
    # Define the limits for the cubes
    limits = Dict(:red => 12, :green => 13, :blue => 14)

    # Parse the input
    games = parse_input("input.txt")

    # Calculate the sum of IDs of possible games
    possible_ids_sum = 0
    for (game_id, game_data) in games
        if is_possible(game_data, limits)
            possible_ids_sum += game_id
        end
    end

    # Print the result
    println("The sum of the IDs of the possible games is: $possible_ids_sum")
end

# Run the main function
main()
