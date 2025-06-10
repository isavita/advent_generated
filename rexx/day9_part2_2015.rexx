
/* REXX */
call main

main:
    distances. = ''
    locations_set. = 0
    all_locations = ''
    inputFile = 'input.txt'

    do while lines(inputFile) > 0
        line = linein(inputFile)
        parse var line loc1 'to' loc2 '=' dist
        loc1 = strip(loc1); loc2 = strip(loc2); dist = strip(dist)

        distances.loc1.loc2 = dist
        distances.loc2.loc1 = dist

        if locations_set.loc1 = 0 then do
            locations_set.loc1 = 1
            all_locations = all_locations loc1
        end
        if locations_set.loc2 = 0 then do
            locations_set.loc2 = 1
            all_locations = all_locations loc2
        end
    end
    call stream inputFile, 'c', 'close'
    all_locations = strip(all_locations)

    shortest_dist = 9E12
    longest_dist = 0

    call find_routes '', all_locations

    say shortest_dist
    say longest_dist
exit

find_routes: procedure expose distances. shortest_dist longest_dist
    parse arg current_path, remaining_locations

    if remaining_locations = '' then do
        total_dist = 0
        do i = 1 to words(current_path) - 1
            loc1 = word(current_path, i)
            loc2 = word(current_path, i + 1)
            total_dist = total_dist + distances.loc1.loc2
        end
        shortest_dist = min(shortest_dist, total_dist)
        longest_dist = max(longest_dist, total_dist)
        return
    end

    do i = 1 to words(remaining_locations)
        next_loc = word(remaining_locations, i)
        new_remaining = strip(subword(remaining_locations, 1, i - 1) subword(remaining_locations, i + 1))
        call find_routes strip(current_path next_loc), new_remaining
    end
return
