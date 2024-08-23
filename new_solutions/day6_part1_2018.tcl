proc manhattanDistance {x1 y1 x2 y2} {
    return [expr {abs($x1 - $x2) + abs($y1 - $y2)}]
}

proc readCoordinates {filename} {
    set coords {}
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        set parts [split $line ","]
        lappend coords [list [string trim [lindex $parts 0]] [string trim [lindex $parts 1]]]
    }
    close $file
    return $coords
}

proc findLargestFiniteArea {coords} {
    set minX [lindex [lindex $coords 0] 0]
    set maxX $minX
    set minY [lindex [lindex $coords 0] 1]
    set maxY $minY

    foreach coord $coords {
        set x [lindex $coord 0]
        set y [lindex $coord 1]
        if {$x < $minX} {set minX $x}
        if {$x > $maxX} {set maxX $x}
        if {$y < $minY} {set minY $y}
        if {$y > $maxY} {set maxY $y}
    }

    set areaCounts [dict create]
    set infiniteCoords [dict create]

    for {set x $minX} {$x <= $maxX} {incr x} {
        for {set y $minY} {$y <= $maxY} {incr y} {
            set minDist [expr {2 * ($maxX - $minX + $maxY - $minY)}]
            set closestCoord {}
            set tie 0
            foreach coord $coords {
                set dist [manhattanDistance $x $y [lindex $coord 0] [lindex $coord 1]]
                if {$dist < $minDist} {
                    set minDist $dist
                    set closestCoord $coord
                    set tie 0
                } elseif {$dist == $minDist} {
                    set tie 1
                }
            }
            if {!$tie && $closestCoord ne {}} {
                dict incr areaCounts $closestCoord
                if {$x == $minX || $x == $maxX || $y == $minY || $y == $maxY} {
                    dict set infiniteCoords $closestCoord 1
                }
            }
        }
    }

    set largestArea 0
    foreach {coord area} [dict get $areaCounts] {
        if {! [dict exists $infiniteCoords $coord]} {
            if {$area > $largestArea} {
                set largestArea $area
            }
        }
    }
    return $largestArea
}

set coords [readCoordinates "input.txt"]
puts [findLargestFiniteArea $coords]