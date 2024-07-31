set input [read [open "input.txt"]]
set initialState [split $input "\n"]
set activeCubes [dict create]

for {set y 0} {$y < [llength $initialState]} {incr y} {
    set line [lindex $initialState $y]
    for {set x 0} {$x < [string length $line]} {incr x} {
        if {[string index $line $x] eq "#"} {
            dict set activeCubes "$x,$y,0" 1
        }
    }
}

proc simulateCycle {activeCubes} {
    set newActiveCubes [dict create]
    set neighborCounts [dict create]

    foreach coord [dict keys $activeCubes] {
        set coords [split $coord ","]
        set x [lindex $coords 0]
        set y [lindex $coords 1]
        set z [lindex $coords 2]
        for {set dz -1} {$dz <= 1} {incr dz} {
            for {set dy -1} {$dy <= 1} {incr dy} {
                for {set dx -1} {$dx <= 1} {incr dx} {
                    if {$dz == 0 && $dy == 0 && $dx == 0} continue
                    set neighbor [format "%d,%d,%d" [expr {$x + $dx}] [expr {$y + $dy}] [expr {$z + $dz}]]
                    dict incr neighborCounts $neighbor
                }
            }
        }
    }

    foreach {coord count} [dict get $neighborCounts] {
        if {$count == 3 || ($count == 2 && [dict exists $activeCubes $coord])} {
            dict set newActiveCubes $coord 1
        }
    }

    return $newActiveCubes
}

for {set cycle 0} {$cycle < 6} {incr cycle} {
    set activeCubes [simulateCycle $activeCubes]
}

puts [dict size $activeCubes]