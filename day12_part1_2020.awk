
#!/usr/bin/awk -f
BEGIN {
    ship_x = 0
    ship_y = 0
    facing = 0
}

{
    action = substr($0, 1, 1)
    value = substr($0, 2)

    if (action == "N") {
        ship_y += value
    } else if (action == "S") {
        ship_y -= value
    } else if (action == "E") {
        ship_x += value
    } else if (action == "W") {
        ship_x -= value
    } else if (action == "L") {
        facing = (facing - value + 360) % 360
    } else if (action == "R") {
        facing = (facing + value) % 360
    } else if (action == "F") {
        if (facing == 0) {
            ship_x += value
        } else if (facing == 90) {
            ship_y -= value
        } else if (facing == 180) {
            ship_x -= value
        } else if (facing == 270) {
            ship_y += value
        }
    }
}

END {
    manhattan_distance = abs(ship_x) + abs(ship_y)
    print manhattan_distance
}

function abs(x) {
    return x < 0 ? -x : x
}
