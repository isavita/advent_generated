mutable struct Marble
    value::Int
    prev::Union{Marble,Nothing}
    next::Union{Marble,Nothing}
end

function read_input(filename)
    open(filename) do file
        line = readline(file)
        parts = split(line)
        players = parse(Int, parts[1])
        last_marble = parse(Int, parts[7])
        return players, last_marble
    end
end

function play_marble_game(players, last_marble)
    scores = zeros(Int, players)
    current = Marble(0, nothing, nothing)
    current.next = current
    current.prev = current

    for marble in 1:last_marble
        if marble % 23 == 0
            player = marble % players + 1
            for _ in 1:7
                current = current.prev
            end
            scores[player] += marble + current.value
            current.prev.next = current.next
            current.next.prev = current.prev
            current = current.next
        else
            current = current.next
            new_marble = Marble(marble, current, current.next)
            current.next.prev = new_marble
            current.next = new_marble
            current = new_marble
        end
    end

    return maximum(scores)
end

players, last_marble = read_input("input.txt")
last_marble *= 100
println(play_marble_game(players, last_marble))