
#!/usr/bin/env tclsh

proc manhattan {p1 p2} {
    return [expr {abs([lindex $p1 0] - [lindex $p2 0]) + abs([lindex $p1 1] - [lindex $p2 1])}]
}

proc impossible {sensors y} {
    set pts [dict create]
    foreach s $sensors {
        lassign $s pos beacon dist
        set dy [expr {abs([lindex $pos 1] - $y)}]
        if {$dy <= $dist} {
            set range [expr {$dist - $dy}]
            for {set x [expr {[lindex $pos 0] - $range}]} {$x <= [expr {[lindex $pos 0] + $range}]} {incr x} {
                dict set pts $x 1
            }
        }
    }

    foreach s $sensors {
        lassign $s pos beacon dist
        if {[lindex $beacon 1] == $y} {
            dict unset pts [lindex $beacon 0]
        }
    }

    return [dict size $pts]
}

# Read input
set input [split [read [open "input.txt" r]] "\n"]

# Parse sensors
set sensors {}
foreach line $input {
    if {$line eq ""} continue
    regexp {Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)} $line -> sx sy bx by
    set dist [manhattan [list $sx $sy] [list $bx $by]]
    lappend sensors [list [list $sx $sy] [list $bx $by] $dist]
}

# Solve and print
puts [impossible $sensors 2000000]
