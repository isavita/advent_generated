
function read_decks(filename)
    player1 = Int[]
    player2 = Int[]
    current_player = 1

    open(filename, "r") do file
        for line in eachline(file)
            if line == ""
                continue
            elseif startswith(line, "Player 1")
                current_player = 1
            elseif startswith(line, "Player 2")
                current_player = 2
            else
                if current_player == 1
                    push!(player1, parse(Int, line))
                else
                    push!(player2, parse(Int, line))
                end
            end
        end
    end

    return player1, player2
end

function play_recursive_combat(deck1, deck2, previous_rounds)
    while !isempty(deck1) && !isempty(deck2)
        round_state = (copy(deck1), copy(deck2))
        if round_state in previous_rounds
            return 1, deck1 # Player 1 wins instantly
        end

        push!(previous_rounds, round_state)

        card1 = popfirst!(deck1)
        card2 = popfirst!(deck2)

        if length(deck1) >= card1 && length(deck2) >= card2
            # Play a sub-game
            sub_deck1 = copy(deck1[1:card1])
            sub_deck2 = copy(deck2[1:card2])
            winner, _ = play_recursive_combat(sub_deck1, sub_deck2, [])
        else
            # Regular round
            winner = card1 > card2 ? 1 : 2
        end

        if winner == 1
            push!(deck1, card1, card2)
        else
            push!(deck2, card2, card1)
        end
    end

    return isempty(deck1) ? 2 : 1, isempty(deck1) ? deck2 : deck1
end

function calculate_score(deck)
    return sum(deck[i] * (length(deck) - i + 1) for i in 1:length(deck))
end

function main()
    player1, player2 = read_decks("input.txt")
    winner, winning_deck = play_recursive_combat(player1, player2, Set())
    score = calculate_score(winning_deck)
    println("Winning player's score: $score")
end

main()
