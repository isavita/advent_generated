
# Tcl solution for the sand simulation (Advent of Code 2022 Day 14 – Part 2)

set fp [open "input.txt"]
set data [read $fp]
close $fp

set occupied [dict create]
set max_y -1

foreach line [split $data "\n"] {
    if {$line eq ""} continue
    set pts {}
    foreach token [split $line " -> "] {
        foreach {x y} [split $token ","] {}
        lappend pts [list $x $y]
        if {$y > $max_y} {set max_y $y}
    }
    for {set i 0} {$i < [llength $pts]-1} {incr i} {
        set p1 [lindex $pts $i]
        set p2 [lindex $pts [expr {$i+1}]]
        set x1 [lindex $p1 0]; set y1 [lindex $p1 1]
        set x2 [lindex $p2 0]; set y2 [lindex $p2 1]
        if {$x1 == $x2} {
            set y0 [expr {$y1<$y2?$y1:$y2}]
            set y1_ [expr {$y1>$y2?$y1:$y2}]
            for {set y $y0} {$y <= $y1_} {incr y} {
                dict set occupied [format "%d,%d" $y $x1] 1
            }
        } else {
            set x0 [expr {$x1<$x2?$x1:$x2}]
            set x1_ [expr {$x1>$x2?$x1:$x2}]
            for {set x $x0} {$x <= $x1_} {incr x} {
                dict set occupied [format "%d,%d" $y1 $x] 1
            }
        }
    }
}

set floor_y [expr {$max_y + 2}]
set sand 0

while {![dict exists $occupied "0,500"]} {
    set r 0
    set c 500
    while {1} {
        if {$r + 1 == $floor_y} break
        if {![dict exists $occupied [format "%d,%d" [expr {$r+1}] $c]]} {
            incr r
            continue
        }
        if {![dict exists $occupied [format "%d,%d" [expr {$r+1}] [expr {$c-1}]]]} {
            incr r; incr c -1
            continue
        }
        if {![dict exists $occupied [format "%d,%d" [expr {$r+1}] [expr {$c+1}]]]} {
            incr r; incr c
            continue
        }
        break
    }
    dict set occupied [format "%d,%d" $r $c] 1
    incr sand
}

puts $sand
