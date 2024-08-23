proc readGrid {filename} {
    set grid {}
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        lappend grid [split $line ""]
    }
    close $file
    return $grid
}

proc countOnNeighbors {grid x y} {
    set count 0
    set directions {-1 -1 -1 0 -1 1 0 -1 0 1 1 -1 1 0 1 1}
    foreach {dx dy} $directions {
        set nx [expr {$x + $dx}]
        set ny [expr {$y + $dy}]
        if {$nx >= 0 && $nx < [llength $grid] && $ny >= 0 && $ny < [llength [lindex $grid 0]]} {
            if {[lindex [lindex $grid $nx] $ny] eq "#"} {
                incr count
            }
        }
    }
    return $count
}

proc stepGrid {grid} {
    set newGrid [list]
    for {set i 0} {$i < [llength $grid]} {incr i} {
        set newRow {}
        for {set j 0} {$j < [llength [lindex $grid 0]]} {incr j} {
            set onNeighbors [countOnNeighbors $grid $i $j]
            set current [lindex [lindex $grid $i] $j]
            if {$current eq "#" && ($onNeighbors == 2 || $onNeighbors == 3)} {
                lappend newRow "#"
            } elseif {$current eq "." && $onNeighbors == 3} {
                lappend newRow "#"
            } else {
                lappend newRow "."
            }
        }
        lappend newGrid $newRow
    }
    return $newGrid
}

proc countLightsOn {grid} {
    set count 0
    foreach row $grid {
        foreach light $row {
            if {$light eq "#"} {
                incr count
            }
        }
    }
    return $count
}

set grid [readGrid "input.txt"]
for {set step 0} {$step < 100} {incr step} {
    set grid [stepGrid $grid]
}
puts [countLightsOn $grid]