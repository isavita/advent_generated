
function solve()
    lines = readlines("input.txt")
    hands = []
    for line in lines
        m = match(r"([\dAKQJT]+) (\d+)", line)
        if m !== nothing
            push!(hands, (m.captures[1], parse(Int, m.captures[2])))
        end
    end

    matches = [[] for _ in 1:7]
    for (cards, bid) in hands
        count = Dict{Char, Int}()
        for card in cards
            count[card] = get(count, card, 0) + 1
        end
        
        value = 1
        for c in values(count)
            value *= c
        end

        if value == 1
            push!(matches[7], (cards, bid))
        elseif value == 2
            push!(matches[6], (cards, bid))
        elseif value == 3
            push!(matches[4], (cards, bid))
        elseif value == 4
            if length(count) == 2
                push!(matches[2], (cards, bid))
            else
                push!(matches[5], (cards, bid))
            end
        elseif value == 5
            push!(matches[1], (cards, bid))
        elseif value == 6
            push!(matches[3], (cards, bid))
        end
    end

    converted_matches = []
    for category in matches
        temp = []
        for (cards, bid) in category
            
            cards_hex = replace(cards, "A" => "E", "T" => "A", "J" => "B", "Q" => "C", "K" => "D")
            num = parse(Int, cards_hex, base=16)
            push!(temp, ((cards, bid), num))
        end
        sort!(temp, by = x -> x[2], rev=true)
        append!(converted_matches, temp)
    end

    total = 0
    for i in 1:length(converted_matches)
        total += converted_matches[i][1][2] * (length(converted_matches) - i + 1)
    end

    println(total)
end

solve()
