# Define a struct to represent the Bots and Outputs
struct Entity
    chips::Vector{Int}
end

# Constructor for creating an empty Entity
Entity() = Entity(Int[])

# Function to process the instructions and simulate the bots' behavior
function process_instructions(filename)
    bots = Dict{Int, Entity}()
    outputs = Dict{Int, Entity}()
    instructions = Dict{Int, Tuple}()

    # Read the file and parse instructions
    for line in readlines(filename)
        if occursin("value", line)
            value, bot_id = parse.(Int, match(r"value (\d+) goes to bot (\d+)", line).captures)
            push!(get!(bots, bot_id, Entity()).chips, value)
        else
            captures = match(r"bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)", line).captures
            bot_id, low_type, low_id, high_type, high_id = captures
            bot_id, low_id, high_id = parse.(Int, [bot_id, low_id, high_id])
            instructions[bot_id] = ((low_type, low_id), (high_type, high_id))
        end
    end

    # Function to give a chip to a bot or output
    function give_chip(entity_type, entity_id, chip)
        if entity_type == "bot"
            push!(get!(bots, entity_id, Entity()).chips, chip)
        else
            push!(get!(outputs, entity_id, Entity()).chips, chip)
        end
    end

    # Process bot actions
    while any(length(entity.chips) == 2 for entity in values(bots))
        for (bot_id, bot) in collect(bots)
            if length(bot.chips) == 2
                sort!(bot.chips)
                if bot.chips == [17, 61]
                    println("Bot responsible for comparing microchips 17 and 61 is bot $bot_id")
                end
                low_chip, high_chip = bot.chips
                low_instr, high_instr = instructions[bot_id]
                give_chip(low_instr[1], low_instr[2], low_chip)
                give_chip(high_instr[1], high_instr[2], high_chip)
                bots[bot_id] = Entity()  # Clear the bot's chips
            end
        end
    end
end

# Main function to run the program
function main()
    process_instructions("input.txt")
end

main()