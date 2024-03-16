using Printf

function main()
    player1Deck, player2Deck = [], []
    currentDeck = player1Deck
    open("input.txt", "r") do file
        for line in eachline(file)
            if line == ""
                currentDeck = player2Deck
            elseif !occursin("Player", line)
                push!(currentDeck, parse(Int, line))
            end
        end
    end

    while !isempty(player1Deck) && !isempty(player2Deck)
        card1, card2 = popfirst!(player1Deck), popfirst!(player2Deck)
        if card1 > card2
            append!(player1Deck, [card1, card2])
        else
            append!(player2Deck, [card2, card1])
        end
    end

    winningDeck = isempty(player1Deck) ? player2Deck : player1Deck
    score = 0
    for (i, card) in enumerate(winningDeck)
        score += card * (length(winningDeck) - i + 1)
    end
    @printf("%d\n", score)
end

main()