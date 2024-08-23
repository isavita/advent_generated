proc readInput {filename} {
    set grid [dict create]
    set y 0
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        for {set x 0} {$x < [string length $line]} {incr x} {
            if {[string index $line $x] eq "#"} {
                dict set grid "$x,$y,0,0" 1
            }
        }
        incr y
    }
    close $file
    return $grid
}

proc getNeighbors {x y z w} {
    set neighbors {}
    for {set dx -1} {$dx <= 1} {incr dx} {
        for {set dy -1} {$dy <= 1} {incr dy} {
            for {set dz -1} {$dz <= 1} {incr dz} {
                for {set dw -1} {$dw <= 1} {incr dw} {
                    if {$dx == 0 && $dy == 0 && $dz == 0 && $dw == 0} continue
                    lappend neighbors [list [expr {$x + $dx}] [expr {$y + $dy}] [expr {$z + $dz}] [expr {$w + $dw}]]
                }
            }
        }
    }
    return $neighbors
}

proc simulateCycle {grid} {
    set newGrid [dict create]
    set toCheck [dict create]

    foreach key [dict keys $grid] {
        set coords [split $key ,]
        set x [lindex $coords 0]
        set y [lindex $coords 1]
        set z [lindex $coords 2]
        set w [lindex $coords 3]

        foreach neighbor [getNeighbors $x $y $z $w] {
            dict incr toCheck [join $neighbor ,]
        }
    }

    foreach key [dict keys $toCheck] {
        set coords [split $key ,]
        set x [lindex $coords 0]
        set y [lindex $coords 1]
        set z [lindex $coords 2]
        set w [lindex $coords 3]

        set activeNeighbors 0
        foreach neighbor [getNeighbors $x $y $z $w] {
            if {[dict exists $grid [join $neighbor ,]]} {
                incr activeNeighbors
            }
        }

        if {[dict exists $grid $key] && ($activeNeighbors == 2 || $activeNeighbors == 3)} {
            dict set newGrid $key 1
        } elseif {! [dict exists $grid $key] && $activeNeighbors == 3} {
            dict set newGrid $key 1
        }
    }

    return $newGrid
}

proc countActiveCubes {grid} {
    return [dict size $grid]
}

set grid [readInput "input.txt"]
for {set i 0} {$i < 6} {incr i} {
    set grid [simulateCycle $grid]
}
puts [countActiveCubes $grid]