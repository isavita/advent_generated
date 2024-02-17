
open("input.txt") do file
    earliestDeparture = parse(Int, readline(file))
    busIDs = split(readline(file), ",")

    earliestBusID = 0
    minWaitTime = earliestDeparture

    for id in busIDs
        if id == "x"
            continue
        end
        busID = parse(Int, id)
        waitTime = busID - (earliestDeparture % busID)
        if waitTime < minWaitTime
            minWaitTime = waitTime
            earliestBusID = busID
        end
    end

    println(earliestBusID * minWaitTime)
end
