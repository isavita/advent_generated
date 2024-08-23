proc readInput {filename} {
    set file [open $filename r]
    set wire1 [split [gets $file] ","]
    set wire2 [split [gets $file] ","]
    close $file
    return [list $wire1 $wire2]
}

proc getPoints {wire} {
    set x 0
    set y 0
    set points {}
    foreach segment $wire {
        set direction [string index $segment 0]
        set length [string range $segment 1 end]
        for {set i 0} {$i < $length} {incr i} {
            switch -- $direction {
                R {incr x}
                L {incr x -1}
                U {incr y}
                D {incr y -1}
            }
            lappend points "$x,$y"
        }
    }
    return $points
}

proc findClosestIntersection {wire1 wire2} {
    set points1 [getPoints $wire1]
    set points2 [getPoints $wire2]
    set set1 [dict create]
    foreach point $points1 {
        dict set set1 $point 1
    }
    set minDistance 2147483647
    foreach point $points2 {
        if {[dict exists $set1 $point]} {
            set coords [split $point ","]
            set x [lindex $coords 0]
            set y [lindex $coords 1]
            set distance [expr {abs($x) + abs($y)}]
            if {$distance < $minDistance} {
                set minDistance $distance
            }
        }
    }
    return $minDistance
}

set wires [readInput "input.txt"]
set wire1 [lindex $wires 0]
set wire2 [lindex $wires 1]
set closestDistance [findClosestIntersection $wire1 $wire2]
puts $closestDistance