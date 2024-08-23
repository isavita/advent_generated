proc readInput {filename} {
    set grid {}
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        lappend grid [split $line ""]
    }
    close $file
    return $grid
}

proc countAdjacent {grid x y type} {
    set count 0
    set directions {-1 -1 -1 0 -1 1 0 -1 0 1 1 -1 1 0 1 1}
    foreach {dx dy} $directions {
        set nx [expr {$x + $dx}]
        set ny [expr {$y + $dy}]
        if {$nx >= 0 && $nx < [llength $grid] && $ny >= 0 && $ny < [llength [lindex $grid 0]]} {
            if {[lindex [lindex $grid $nx] $ny] eq $type} {
                incr count
            }
        }
    }
    return $count
}

proc simulateMinute {grid} {
    set newGrid [list]
    for {set i 0} {$i < [llength $grid]} {incr i} {
        set newRow [list]
        for {set j 0} {$j < [llength [lindex $grid $i]]} {incr j} {
            set acre [lindex [lindex $grid $i] $j]
            switch -- $acre {
                "." {
                    set trees [countAdjacent $grid $i $j "|"]
                    if {$trees >= 3} {
                        lappend newRow "|"
                    } else {
                        lappend newRow "."
                    }
                }
                "|" {
                    set lumberyards [countAdjacent $grid $i $j "#"]
                    if {$lumberyards >= 3} {
                        lappend newRow "#"
                    } else {
                        lappend newRow "|"
                    }
                }
                "#" {
                    set lumberyards [countAdjacent $grid $i $j "#"]
                    set trees [countAdjacent $grid $i $j "|"]
                    if {$lumberyards >= 1 && $trees >= 1} {
                        lappend newRow "#"
                    } else {
                        lappend newRow "."
                    }
                }
            }
        }
        lappend newGrid $newRow
    }
    return $newGrid
}

proc calculateResourceValue {grid} {
    set wooded 0
    set lumberyards 0
    foreach row $grid {
        foreach acre $row {
            switch -- $acre {
                "|" {incr wooded}
                "#" {incr lumberyards}
            }
        }
    }
    return [expr {$wooded * $lumberyards}]
}

set grid [readInput "input.txt"]
for {set minute 0} {$minute < 10} {incr minute} {
    set grid [simulateMinute $grid]
}
puts [calculateResourceValue $grid]