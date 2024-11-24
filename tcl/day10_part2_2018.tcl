
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
            set newX [expr {$x + $vx * $t}]
            set newY [expr {$y + $vy * $t}]
            lappend coords [list $newX $newY]
        }

        set minX [tcl::mathfunc::min {*}[lmap coord $coords {lindex $coord 0}]]
        set maxX [tcl::mathfunc::max {*}[lmap coord $coords {lindex $coord 0}]]
        set minY [tcl::mathfunc::min {*}[lmap coord $coords {lindex $coord 1}]]
        set maxY [tcl::mathfunc::max {*}[lmap coord $coords {lindex $coord 1}]]

        set lenX [expr {$maxX - $minX + 1}]
        set lenY [expr {$maxY - $minY + 1}]
        set area [expr {$lenX * $lenY}]

        if {$area < $smallestArea} {
            set smallestArea $area
            set smallestT $t
        }
    }

    puts $smallestT
}

findMessage
