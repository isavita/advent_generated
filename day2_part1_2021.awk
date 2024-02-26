
BEGIN {
    FS = " "
    horiz = 0
    depth = 0
}

{
    if ($1 == "forward") {
        horiz += $2
    } else if ($1 == "down") {
        depth += $2
    } else if ($1 == "up") {
        depth -= $2
    }
}

END {
    print horiz * depth
}
