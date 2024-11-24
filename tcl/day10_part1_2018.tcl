
#!/usr/bin/env tclsh

proc toInt {s} {
    return [expr {int($s)}]
}

proc findMessage {} {
    set input [open "input.txt" r]
    set stars {}
    
    while {[gets $input line] != -1} {
        if {[regexp {position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>} $line -> x y vx vy]} {
            lappend stars [list $x $y $vx $vy]
        }
    }
    close $input

    set smallestT 0
    set smallestArea [expr {2**31 - 1}]

    for {set t 1} {$t < 100000} {incr t} {
        set coords {}
        foreach star $stars {
            lassign $star x y vx vy
            lappend coords [list [expr {$x + $vx * $t}] [expr {$y + $vy * $t}]]
        }

        set minX [tcl::mathfunc::min {*}[lmap coord $coords {lindex $coord 0}]]
        set maxX [tcl::mathfunc::max {*}[lmap coord $coords {lindex $coord 0}]]
        set minY [tcl::mathfunc::min {*}[lmap coord $coords {lindex $coord 1}]]
        set maxY [tcl::mathfunc::max {*}[lmap coord $coords {lindex $coord 1}]]

        set area [expr {($maxX - $minX + 1) + ($maxY - $minY + 1)}]

        if {$area < $smallestArea} {
            set smallestArea $area
            set smallestT $t
        }
    }

    set finalCoords {}
    foreach star $stars {
        lassign $star x y vx vy
        lappend finalCoords [list [expr {$x + $vx * $smallestT}] [expr {$y + $vy * $smallestT}]]
    }

    set minX [tcl::mathfunc::min {*}[lmap coord $finalCoords {lindex $coord 0}]]
    set maxX [tcl::mathfunc::max {*}[lmap coord $finalCoords {lindex $coord 0}]]
    set minY [tcl::mathfunc::min {*}[lmap coord $finalCoords {lindex $coord 1}]]
    set maxY [tcl::mathfunc::max {*}[lmap coord $finalCoords {lindex $coord 1}]]

    set grid {}
    for {set y $minY} {$y <= $maxY} {incr y} {
        set row {}
        for {set x $minX} {$x <= $maxX} {incr x} {
            lappend row " "
        }
        lappend grid $row
    }

    foreach coord $finalCoords {
        lassign $coord x y
        set row [expr {$y - $minY}]
        set col [expr {$x - $minX}]
        lset grid $row $col "#"
    }

    foreach row $grid {
        puts [join $row ""]
    }

    return $smallestT
}

puts "Time taken: [findMessage]"
