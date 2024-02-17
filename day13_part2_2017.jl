
function passThrough(firewall, delay)
    for (depth, scanner) in firewall
        if (depth + delay) % (2 * (scanner.Range - 1)) == 0
            return false
        end
    end
    return true
end

function main()
    file = open("input.txt", "r")
    firewall = Dict{Int, NamedTuple{(:Range, :Position, :Direction), Tuple{Int, Int, Int}}}()

    for line in eachline(file)
        depth, rng = split(line, ": ")
        depth = parse(Int, depth)
        rng = parse(Int, rng)
        firewall[depth] = (Range=rng, Position=0, Direction=1)
    end

    delay = 0
    while true
        if passThrough(firewall, delay)
            break
        end
        delay += 1
    end

    println(delay)
    close(file)
end

main()
