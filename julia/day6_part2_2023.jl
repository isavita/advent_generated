
function calculate_ways_to_win_long_race(time, record)
    ways_to_win = 0
    for hold_time in 1:(time - 1)
        travel_time = time - hold_time
        distance = hold_time * travel_time
        if distance > record
            ways_to_win += 1
        end
    end
    return ways_to_win
end

function main()
    open("input.txt", "r") do file
        lines = readlines(file)
        time = parse(Int, replace(split(lines[1], ":")[2], " " => ""))
        distance = parse(Int, replace(split(lines[2], ":")[2], " " => ""))
        println(calculate_ways_to_win_long_race(time, distance))
    end
end

main()
