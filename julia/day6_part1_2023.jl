
function solve_race(time, record_distance)
    ways_to_win = 0
    for hold_time in 0:time
        travel_time = time - hold_time
        distance = hold_time * travel_time
        if distance > record_distance
            ways_to_win += 1
        end
    end
    return ways_to_win
end

function main()
    # Read input from file
    lines = readlines("input.txt")
    
    # Parse times and distances
    times = parse.(Int, split(lines[1])[2:end])
    distances = parse.(Int, split(lines[2])[2:end])
    
    # Calculate ways to win for each race and multiply
    result = 1
    for (time, distance) in zip(times, distances)
        result *= solve_race(time, distance)
    end
    
    # Print the final result
    println(result)
end

main()
