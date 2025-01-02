
function play_marble_game(players, last_marble)
    scores = zeros(Int, players)
    circle = [0]
    current_index = 1

    for marble = 1:last_marble
        if marble % 23 == 0
            player = (marble % players) + 1
            remove_index = mod1(current_index - 7, length(circle))
            scores[player] += marble + circle[remove_index]
            deleteat!(circle, remove_index)
            current_index = remove_index
        else
            insert_index = mod1(current_index + 1, length(circle)) + 1
            insert!(circle, insert_index, marble)
            current_index = insert_index
        end
    end

    return maximum(scores)
end

function read_input(filename)
    line = readlines(filename)[1]
    parts = split(line)
    players = parse(Int, parts[1])
    last_marble = parse(Int, parts[7])
    return players, last_marble
end

players, last_marble = read_input("input.txt")
println(play_marble_game(players, last_marble))
