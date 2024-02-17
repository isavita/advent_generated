
struct Disc
    totalPositions::Int
    startPosition::Int
end

function checkDiscs(discs, time)
    for (i, disc) in enumerate(discs)
        position = (disc.startPosition + time + i) % disc.totalPositions
        if position != 0
            return false
        end
    end
    return true
end

discs = Disc[]
open("input.txt") do file
    for line in eachline(file)
        m = match(r"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).", line)
        totalPositions = parse(Int, m.captures[2])
        startPosition = parse(Int, m.captures[3])
        push!(discs, Disc(totalPositions, startPosition))
    end
end

global time = 0
while true
    if checkDiscs(discs, time)
        println(time)
        break
    end
    global time += 1
end
