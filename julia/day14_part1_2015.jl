
mutable struct Reindeer
    speed::Int
    flyTime::Int
    restTime::Int
    distance::Int
    flying::Bool
    timeInMode::Int
end

function main()
    reindeers = readReindeerDetails("input.txt")
    if reindeers == nothing
        println("Error reading input")
        return
    end
    
    simulateRace(reindeers, 2503)
    maxDistance = findMaxDistance(reindeers)
    println(maxDistance)
end

function readReindeerDetails(filename::String)
    reindeers = []
    file = open(filename, "r")
    for line in eachline(file)
        parts = split(line)
        speed = parse(Int, parts[4])
        flyTime = parse(Int, parts[7])
        restTime = parse(Int, parts[14])
        push!(reindeers, Reindeer(speed, flyTime, restTime, 0, true, 0))
    end
    close(file)
    return reindeers
end

function simulateRace(reindeers, totalSeconds)
    for _ in 1:totalSeconds
        for reindeer in reindeers
            if reindeer.flying
                reindeer.distance += reindeer.speed
                reindeer.timeInMode += 1
                if reindeer.timeInMode == reindeer.flyTime
                    reindeer.flying = false
                    reindeer.timeInMode = 0
                end
            else
                reindeer.timeInMode += 1
                if reindeer.timeInMode == reindeer.restTime
                    reindeer.flying = true
                    reindeer.timeInMode = 0
                end
            end
        end
    end
end

function findMaxDistance(reindeers)
    maxDistance = 0
    for reindeer in reindeers
        if reindeer.distance > maxDistance
            maxDistance = reindeer.distance
        end
    end
    return maxDistance
end

main()
