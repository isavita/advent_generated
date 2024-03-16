function play_game(cups, moves)
    cups = [parse(Int, c) for c in cups]
    max_cup = maximum(cups)
    min_cup = minimum(cups)

    next_cup = Dict{Int, Int}()
    for i in 1:length(cups)-1
        next_cup[cups[i]] = cups[i+1]
    end
    next_cup[cups[end]] = cups[1]

    current_cup = cups[1]

    for _ in 1:moves
        cup1 = next_cup[current_cup]
        cup2 = next_cup[cup1]
        cup3 = next_cup[cup2]

        next_cup[current_cup] = next_cup[cup3]

        destination_cup = current_cup - 1
        while destination_cup in [cup1, cup2, cup3] || destination_cup < min_cup
            destination_cup -= 1
            if destination_cup < min_cup
                destination_cup = max_cup
            end
        end

        temp = next_cup[destination_cup]
        next_cup[destination_cup] = cup1
        next_cup[cup3] = temp

        current_cup = next_cup[current_cup]
    end

    cup = 1
    result = ""
    for _ in 1:length(cups)-1
        cup = next_cup[cup]
        result *= string(cup)
    end
    return result
end

input = readlines("input.txt")[1]

result = play_game(input, 100)
println(result)