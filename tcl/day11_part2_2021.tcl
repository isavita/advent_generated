
#!/usr/bin/tclsh

proc readInput {filename} {
    set fileId [open $filename r]
    set lines [split [read $fileId] "\n"]
    close $fileId
    set grid {}
    foreach line $lines {
        if {$line eq ""} continue
        set row {}
        foreach char [split $line ""] {
            lappend row [expr {$char}]
        }
        lappend grid $row
    }
    return $grid
}

proc flash {gridName x y flashed_coordsName} {
    upvar 1 $gridName grid
    upvar 1 $flashed_coordsName flashed_coords

    set key "$x,$y"
    if {[info exists flashed_coords($key)]} {
        return 0
    }

    set flashed_coords($key) 1
    set flashes 1

    set directions {-1 -1 -1 0 -1 1 0 -1 0 1 1 -1 1 0 1 1}

    set gridHeight [llength $grid]
    set gridWidth [llength [lindex $grid 0]]

    for {set i 0} {$i < [llength $directions]} {incr i 2} {
        set dx [lindex $directions $i]
        set dy [lindex $directions [expr {$i+1}]]

        set newX [expr {$x + $dx}]
        set newY [expr {$y + $dy}]

        if {$newX >= 0 && $newX < $gridWidth && $newY >= 0 && $newY < $gridHeight} {
            set energy [lindex $grid $newY $newX]
            set energy [expr {$energy + 1}]
            lset grid $newY $newX $energy

            set neighborKey "$newX,$newY"
            if {$energy > 9 && ![info exists flashed_coords($neighborKey)]} {
                set flashes [expr {$flashes + [flash $gridName $newX $newY $flashed_coordsName]}]
            }
        }
    }

    return $flashes
}

proc simulateStep {gridName} {
    upvar 1 $gridName grid
    set totalFlashes 0
    array unset flashed_coords

    set gridHeight [llength $grid]
    set gridWidth [llength [lindex $grid 0]]

    for {set y 0} {$y < $gridHeight} {incr y} {
        for {set x 0} {$x < $gridWidth} {incr x} {
            set energy [lindex $grid $y $x]
            lset grid $y $x [expr {$energy + 1}]
        }
    }

    for {set y 0} {$y < $gridHeight} {incr y} {
        for {set x 0} {$x < $gridWidth} {incr x} {
            set energy [lindex $grid $y $x]
            set key "$x,$y"
            if {$energy > 9 && ![info exists flashed_coords($key)]} {
                set totalFlashes [expr {$totalFlashes + [flash $gridName $x $y flashed_coords]}]
            }
        }
    }

    foreach key [array names flashed_coords] {
        set coords [split $key ","]
        set x [lindex $coords 0]
        set y [lindex $coords 1]
        lset grid $y $x 0
    }

    return $totalFlashes
}

proc main {} {
    set grid [readInput "input.txt"]
    set step 0

    while {true} {
        incr step
        set flashes [simulateStep grid]
        if {$flashes == 100} {
            puts $step
            break
        }
    }
}

main
