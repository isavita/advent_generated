set file [open "input.txt" r]
set seatingArea [list]

while {[gets $file line] >= 0} {
    lappend seatingArea [split $line ""]
}
close $file

proc simulateSeating {seatingArea} {
    set rows [llength $seatingArea]
    set cols [llength [lindex $seatingArea 0]]
    set newSeatingArea [list]
    set stabilized 1

    for {set i 0} {$i < $rows} {incr i} {
        set newRow [lindex $seatingArea $i]
        for {set j 0} {$j < $cols} {incr j} {
            switch [lindex $newRow $j] {
                "L" {
                    if {[countAdjacentOccupied $seatingArea $i $j] == 0} {
                        set newRow [lreplace $newRow $j $j "#"]
                        set stabilized 0
                    }
                }
                "#" {
                    if {[countAdjacentOccupied $seatingArea $i $j] >= 4} {
                        set newRow [lreplace $newRow $j $j "L"]
                        set stabilized 0
                    }
                }
            }
        }
        lappend newSeatingArea $newRow
    }
    return [list $newSeatingArea $stabilized]
}

proc countAdjacentOccupied {seatingArea row col} {
    set count 0
    set directions {-1 -1 -1 0 -1 1 0 -1 0 1 1 -1 1 0 1 1}
    foreach {dx dy} $directions {
        set newRow [expr {$row + $dx}]
        set newCol [expr {$col + $dy}]
        if {$newRow >= 0 && $newRow < [llength $seatingArea] && $newCol >= 0 && $newCol < [llength [lindex $seatingArea 0]]} {
            if {[lindex [lindex $seatingArea $newRow] $newCol] == "#"} {
                incr count
            }
        }
    }
    return $count
}

proc countOccupiedSeats {seatingArea} {
    set count 0
    foreach row $seatingArea {
        foreach seat $row {
            if {$seat == "#"} {
                incr count
            }
        }
    }
    return $count
}

set stabilized 0
while {!$stabilized} {
    set result [simulateSeating $seatingArea]
    set seatingArea [lindex $result 0]
    set stabilized [lindex $result 1]
}

puts [countOccupiedSeats $seatingArea]