mutable struct Bot
    lowTo::String
    highTo::String
    chips::Vector{Int}
end

function main()
    bots = Dict{String,Bot}()
    outputs = Dict{String,Int}()
    value_regex = r"value (\d+) goes to (bot \d+)"
    gives_regex = r"(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)"

    open("input.txt", "r") do file
        for line in eachline(file)
            if match(value_regex, line) !== nothing
                value, botID = parse(Int, match(value_regex, line)[1]), match(value_regex, line)[2]
                if haskey(bots, botID)
                    push!(bots[botID].chips, value)
                else
                    bots[botID] = Bot("", "", [value])
                end
            elseif match(gives_regex, line) !== nothing
                botID, lowTo, highTo = match(gives_regex, line)[1], match(gives_regex, line)[2], match(gives_regex, line)[3]
                if haskey(bots, botID)
                    bots[botID].lowTo = lowTo
                    bots[botID].highTo = highTo
                else
                    bots[botID] = Bot(lowTo, highTo, [])
                end
            end
        end
    end

    while true
        action = false
        for (botID, bot) in bots
            if length(bot.chips) == 2
                action = true
                low, high = minmax(bot.chips[1], bot.chips[2])
                bot.chips = []
                giveChip(bots, outputs, bot.lowTo, low)
                giveChip(bots, outputs, bot.highTo, high)
            end
        end
        if !action
            break
        end
    end

    result = outputs["output 0"] * outputs["output 1"] * outputs["output 2"]
    println(result)
end

function giveChip(bots, outputs, target, value)
    if startswith(target, "bot")
        if !haskey(bots, target)
            bots[target] = Bot("", "", [])
        end
        push!(bots[target].chips, value)
    elseif startswith(target, "output")
        outputs[target] = value
    end
end

function minmax(a, b)
    if a < b
        return a, b
    else
        return b, a
    end
end

main()