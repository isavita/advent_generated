function simulate_seating(seating_area)
    rows, cols = size(seating_area)
    new_seating_area = copy(seating_area)
    stabilized = true

    for i in 1:rows
        for j in 1:cols
            if seating_area[i, j] == 'L'
                if count_adjacent_occupied(seating_area, i, j) == 0
                    new_seating_area[i, j] = '#'
                    stabilized = false
                end
            elseif seating_area[i, j] == '#'
                if count_adjacent_occupied(seating_area, i, j) >= 4
                    new_seating_area[i, j] = 'L'
                    stabilized = false
                end
            end
        end
    end

    return new_seating_area, stabilized
end

function count_adjacent_occupied(seating_area, row, col)
    count = 0
    rows, cols = size(seating_area)

    for i in max(1, row-1):min(row+1, rows)
        for j in max(1, col-1):min(col+1, cols)
            if i == row && j == col
                continue
            end
            if seating_area[i, j] == '#'
                count += 1
            end
        end
    end

    return count
end

function count_occupied_seats(seating_area)
    count = 0
    for seat in seating_area
        if seat == '#'
            count += 1
        end
    end
    return count
end

seating_area = permutedims(reduce(hcat, collect.(eachline("input.txt"))))
stabilized = false

while !stabilized
    global seating_area, stabilized = simulate_seating(seating_area)
end

println(count_occupied_seats(seating_area))