using Dates

valueDict = Dict('J' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9, 'T' => 10, 'Q' => 11, 'K' => 12, 'A' => 13)

struct Hand
    cards::String
    bid::Int
end

function main()
    file = open("input.txt", "r")
    input = read(file, String)
    close(file)

    lines = split(input, "\n")

    hands = Hand[]

    r"[\dAKQJT]+" |> r -> for line in lines
        if length(line) == 0
            continue
        end

        cards = match(r, line).match
        bid = parse(Int, match(r" [\d]+", line).match[2:end])

        push!(hands, Hand(cards, bid))
    end

    matches = [[] for _ in 1:7]

    for hand in hands
        count = Dict{Char,Int}()

        for i in hand.cards
            if haskey(count, i)
                count[i] += 1
            else
                count[i] = 1
            end
        end

        if haskey(count, 'J')
            highV = 0
            highKey = 'J'
            for y in keys(count)
                if y != 'J'
                    if count[y] > highV
                        highKey = y
                        highV = count[y]
                    elseif count[y] == highV && valueDict[y] > valueDict[highKey]
                        highKey = y
                    end
                end
            end
            if highKey != 'J'
                count[highKey] += count['J']
                delete!(count, 'J')
            end
        end

        value = 1
        for i in values(count)
            value *= i
        end

        if value == 1
            push!(matches[7], hand)
        elseif value == 2
            push!(matches[6], hand)
        elseif value == 3
            push!(matches[4], hand)
        elseif value == 4
            if length(count) == 2
                push!(matches[2], hand)
            else
                push!(matches[5], hand)
            end
        elseif value == 5
            push!(matches[1], hand)
        elseif value == 6
            push!(matches[3], hand)
        else
            println("oh no")
        end
    end

    convertedMatches = []

    for x in matches
        temp = []
        for i in x
            y = replace(i.cards, "A" => "E", "T" => "A", "J" => "1", "Q" => "C", "K" => "D")
            val = parse(Int, y, base=16)
            push!(temp, [val, i.bid])
        end
        sort!(temp, by=x->x[1], rev=true)
        append!(convertedMatches, temp)
    end

    total = 0
    for x in 1:length(convertedMatches)
        total += convertedMatches[x][2] * (length(convertedMatches) - x + 1)
    end

    println(total)
end

main()