using Printf

struct Disc
    totalPositions::Int
    startPosition::Int
end

function main()
    discs = Disc[]
    file = open("input.txt", "r")
    for line in eachline(file)
        matches = match(r"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).", line)
        totalPositions = parse(Int, matches.captures[2])
        startPosition = parse(Int, matches.captures[3])
        push!(discs, Disc(totalPositions, startPosition))
    end
    close(file)

    push!(discs, Disc(11, 0))

    time = 0
    while true
        if checkDiscs(discs, time)
            println(time)
            break
        end
        time += 1
    end
end

function checkDiscs(discs::Vector{Disc}, time::Int)
    for (i, disc) in enumerate(discs)
        position = (disc.startPosition + time + i) % disc.totalPositions
        if position != 0
            return false
        end
    end
    return true
end

main()