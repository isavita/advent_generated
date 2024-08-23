proc readInput {filename} {
    set file [open $filename r]
    set grid [split [read $file] "\n"]
    close $file
    return $grid
}

proc countVisibleOccupied {grid i j} {
    set directions {{-1 -1} {-1 0} {-1 1} {0 -1} {0 1} {1 -1} {1 0} {1 1}}
    set count 0
    foreach dir $directions {
        set x [expr {$i + [lindex $dir 0]}]
        set y [expr {$j + [lindex $dir 1]}]
        while {$x >= 0 && $x < [llength $grid] && $y >= 0 && $y < [string length [lindex $grid 0]]} {
            if {[string index [lindex $grid $x] $y] eq "#"} {
                incr count
                break
            } elseif {[string index [lindex $grid $x] $y] eq "L"} {
                break
            }
            set x [expr {$x + [lindex $dir 0]}]
            set y [expr {$y + [lindex $dir 1]}]
        }
    }
    return $count
}

proc simulateSeating {grid} {
    set changes 1
    while {$changes} {
        set changes 0
        set newGrid {}
        for {set i 0} {$i < [llength $grid]} {incr i} {
            set newRow ""
            for {set j 0} {$j < [string length [lindex $grid $i]]} {incr j} {
                set seat [string index [lindex $grid $i] $j]
                if {$seat eq "L" && [countVisibleOccupied $grid $i $j] == 0} {
                    append newRow "#"
                    set changes 1
                } elseif {$seat eq "#" && [countVisibleOccupied $grid $i $j] >= 5} {
                    append newRow "L"
                    set changes 1
                } else {
                    append newRow $seat
                }
            }
            lappend newGrid $newRow
        }
        set grid $newGrid
    }
    return $grid
}

proc countOccupiedSeats {grid} {
    set count 0
    foreach row $grid {
        foreach seat [split $row ""] {
            if {$seat eq "#"} {
                incr count
            }
        }
    }
    return $count
}

set grid [readInput "input.txt"]
set finalGrid [simulateSeating $grid]
puts [countOccupiedSeats $finalGrid]