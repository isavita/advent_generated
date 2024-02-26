
{
    row = 0
    col = 0
    for (i = 1; i <= 7; i++) {
        if (substr($0, i, 1) == "B") {
            row = row * 2 + 1
        } else {
            row = row * 2
        }
    }
    for (i = 8; i <= 10; i++) {
        if (substr($0, i, 1) == "R") {
            col = col * 2 + 1
        } else {
            col = col * 2
        }
    }
    id = row * 8 + col
    if (id > max_id) {
        max_id = id
    }
    seats[id] = 1
}
END {
    for (i = 1; i <= max_id; i++) {
        if (seats[i] != 1 && seats[i - 1] == 1 && seats[i + 1] == 1) {
            print i
            break
        }
    }
}
