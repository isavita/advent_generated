proc readInput {filename} {
    set grid {}
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        lappend grid [split $line ""]
    }
    close $file
    return $grid
}

proc calculateBiodiversity {grid} {
    set rating 0
    for {set i 0} {$i < 5} {incr i} {
        for {set j 0} {$j < 5} {incr j} {
            if {[lindex [lindex $grid $i] $j] eq "#"} {
                set rating [expr {$rating + (1 << ($i * 5 + $j))}]
            }
        }
    }
    return $rating
}

proc countAdjacentBugs {grid x y} {
    set count 0
    foreach {dx dy} {-1 0 1 0 0 -1 0 1} {
        set nx [expr {$x + $dx}]
        set ny [expr {$y + $dy}]
        if {$nx >= 0 && $nx < 5 && $ny >= 0 && $ny < 5} {
            if {[lindex [lindex $grid $nx] $ny] eq "#"} {
                incr count
            }
        }
    }
    return $count
}

proc nextGeneration {grid} {
    set newGrid [list]
    for {set i 0} {$i < 5} {incr i} {
        set newRow [list]
        for {set j 0} {$j < 5} {incr j} {
            set bugs [countAdjacentBugs $grid $i $j]
            set current [lindex [lindex $grid $i] $j]
            if {$current eq "#" && $bugs != 1} {
                lappend newRow "."
            } elseif {$current eq "." && ($bugs == 1 || $bugs == 2)} {
                lappend newRow "#"
            } else {
                lappend newRow $current
            }
        }
        lappend newGrid $newRow
    }
    return $newGrid
}

proc findFirstDuplicateLayout {grid} {
    set seen [dict create]
    while {1} {
        set rating [calculateBiodiversity $grid]
        if {[dict exists $seen $rating]} {
            return $rating
        }
        dict set seen $rating 1
        set grid [nextGeneration $grid]
    }
}

set grid [readInput "input.txt"]
set result [findFirstDuplicateLayout $grid]
puts $result