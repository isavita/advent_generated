set file [open "input.txt" r]
set directions [read $file]
close $file

set visitedHouses {}
set xSanta 0
set ySanta 0
set xRobo 0
set yRobo 0
set isSantaTurn 1

proc markVisited {houses x y} {
    set key "$x,$y"
    dict set houses $key 1
    return $houses
}

set visitedHouses [markVisited $visitedHouses $xSanta $ySanta]

foreach dir [split $directions ""] {
    if {$isSantaTurn} {
        set x [set xSanta]
        set y [set ySanta]
    } else {
        set x [set xRobo]
        set y [set yRobo]
    }

    switch -- $dir {
        "^" { incr y }
        "v" { incr y -1 }
        ">" { incr x }
        "<" { incr x -1 }
    }

    if {$isSantaTurn} {
        set xSanta $x
        set ySanta $y
    } else {
        set xRobo $x
        set yRobo $y
    }

    set visitedHouses [markVisited $visitedHouses $x $y]
    set isSantaTurn [expr {!$isSantaTurn}]
}

puts [dict size $visitedHouses]