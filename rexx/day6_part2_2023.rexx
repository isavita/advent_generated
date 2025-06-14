
/* Rexx */
call main
exit

main:
    numeric digits 30
    time_line = linein('input.txt')
    dist_line = linein('input.txt')
    call stream 'input.txt', 'c', 'close'

    time = space(substr(time_line, pos(':', time_line) + 1), 0)
    distance = space(substr(dist_line, pos(':', dist_line) + 1), 0)

    say calculateWaysToWin(time, distance)
return

calculateWaysToWin: procedure
    parse arg time, record

    low = 0
    high = time
    do while low <= high
        mid = (low + high) % 2
        d = mid * (time - mid)
        if d > record then
            high = mid - 1
        else
            low = mid + 1
    end
    firstWin = low

    low = 0
    high = time
    do while low <= high
        mid = (low + high) % 2
        d = mid * (time - mid)
        if d > record then
            low = mid + 1
        else
            high = mid - 1
    end
    lastWin = high

    return lastWin - firstWin + 1
