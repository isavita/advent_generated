
mutable struct Reindeer
    speed::Int
    flyTime::Int
    restTime::Int
    distance::Int
    points::Int
    flying::Bool
    timeInMode::Int
end

function main()
    reindeers = readReindeerDetails("input.txt")
    simulateRaceWithPoints(reindeers, 2503)
    maxPoints = findMaxPoints(reindeers)
    println(maxPoints)
end

function readReindeerDetails(filename::String)
    reindeers = Reindeer[]
    for line in eachline(filename)
        parts = split(line)
        speed = parse(Int, parts[4])
        flyTime = parse(Int, parts[7])
        restTime = parse(Int, parts[14])
        push!(reindeers, Reindeer(speed, flyTime, restTime, 0, 0, true, 0))
    end
    return reindeers
end

function simulateRaceWithPoints(reindeers::Array{Reindeer,1}, totalSeconds::Int)
    for _ in 1:totalSeconds
        maxDistance = 0
        for reindeer in reindeers
            if reindeer.flying
                reindeer.distance += reindeer.speed
            end
            reindeer.timeInMode += 1
            if (reindeer.flying && reindeer.timeInMode == reindeer.flyTime) || (!reindeer.flying && reindeer.timeInMode == reindeer.restTime)
                reindeer.flying = !reindeer.flying
                reindeer.timeInMode = 0
            end
            if reindeer.distance > maxDistance
                maxDistance = reindeer.distance
            end
        end
        for reindeer in reindeers
            if reindeer.distance == maxDistance
                reindeer.points += 1
            end
        end
    end
end

function findMaxPoints(reindeers::Array{Reindeer,1})
    maxPoints = 0
    for reindeer in reindeers
        if reindeer.points > maxPoints
            maxPoints = reindeer.points
        end
    end
    return maxPoints
end

main()
