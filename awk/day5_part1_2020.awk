
{
    row = binary_search(substr($0, 1, 7), 0, 127, "F", "B")
    col = binary_search(substr($0, 8, 3), 0, 7, "L", "R")
    seat_id = row * 8 + col
    if (seat_id > max_seat_id) {
        max_seat_id = seat_id
    }
}

END {
    print max_seat_id
}

function binary_search(code, min, max, lower_char, upper_char) {
    for (i = 1; i <= length(code); i++) {
        mid = int((min + max) / 2)
        if (substr(code, i, 1) == lower_char) {
            max = mid
        } else {
            min = mid + 1
        }
    }
    return min
}
