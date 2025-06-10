
/* Rexx */
call main
exit

main:
    time = 2503
    max_dist = 0
    fileName = "input.txt"

    do while lines(fileName) > 0
        line = linein(fileName)
        parse var line . ' can fly ' speed ' km/s for ' fly_time ,
            ' seconds, but then must rest for ' rest_time ' seconds.'

        dist = calculate_distance(speed, fly_time, rest_time, time)
        max_dist = max(max_dist, dist)
    end
    call lineout fileName

    say max_dist
return

calculate_distance:
    parse arg speed, fly_time, rest_time, total_time

    cycle_time = fly_time + rest_time
    full_cycles = total_time % cycle_time
    remaining_time = total_time // cycle_time

    distance = (full_cycles * speed * fly_time) + (min(remaining_time, fly_time) * speed)
return distance
