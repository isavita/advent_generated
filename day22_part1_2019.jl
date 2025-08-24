const Size = 10007

function dealIntoNewStack(deck)
    for i in 1:div(Size, 2)
        deck[i], deck[Size-i+1] = deck[Size-i+1], deck[i]
    end
    return deck
end

function cutN(deck, n)
    if n >= 0
        return vcat(deck[n+1:end], deck[1:n])
    else
        return vcat(deck[end+n+1:end], deck[1:end+n])
    end
end

function dealWithIncrement(deck, n)
    newDeck = zeros(Int, length(deck))
    for i in 1:Size
        newDeck[((i-1)*n % Size)+1] = deck[i]
    end
    return newDeck
end

function find2019(deck)
    for i in 1:Size
        if deck[i] == 2019
            return i-1
        end
    end
    return -1
end

function main()
    deck = collect(0:Size-1)

    open("input.txt") do file
        for line in eachline(file)
            if line == "deal into new stack"
                deck = dealIntoNewStack(deck)
                continue
            end

            if startswith(line, "cut")
                n = parse(Int, split(line)[2])
                deck = cutN(deck, n)
                continue
            end

            if startswith(line, "deal with increment")
                n = parse(Int, split(line)[end])
                deck = dealWithIncrement(deck, n)
                continue
            end
        end
    end

    println(find2019(deck))
end

main()