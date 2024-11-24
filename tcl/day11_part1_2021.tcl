
#!/usr/bin/env tclsh

proc readInput {filename} {
    set grid {}
    set file [open $filename r]
    while {[gets $file line] != -1} {
        set row {}
        foreach char [split $line ""] {
            lappend row [scan $char %d]
        }
        lappend grid $row
    }
    close $file
    return $grid
}

proc simulateStep {gridVar} {
    upvar $gridVar grid
    set flashes 0
    set flashed {}
    set height [llength $grid]
    set width [llength [lindex $grid 0]]

    # Increase energy by 1 for all octopuses
    for {set y 0} {$y < $height} {incr y} {
        for {set x 0} {$x < $width} {incr x} {
            lset grid $y $x [expr {[lindex $grid $y $x] + 1}]
        }
    }

    # Flash octopuses with energy greater than 9
    for {set y 0} {$y < $height} {incr y} {
        for {set x 0} {$x < $width} {incr x} {
            if {[lindex $grid $y $x] > 9} {
                incr flashes [flash grid $x $y flashed]
            }
        }
    }

    # Reset energy to 0 for all that flashed
    foreach coords $flashed {
        lset grid [lindex $coords 1] [lindex $coords 0] 0
    }

    return $flashes
}

proc flash {gridVar x y flashedVar} {
    upvar $gridVar grid
    upvar $flashedVar flashed
    set height [llength $grid]
    set width [llength [lindex $grid 0]]

    set key [list $x $y]
    if {[lsearch -exact $flashed $key] != -1} {
        return 0
    }

    lappend flashed $key
    set flashes 1
    set directions {{-1 -1} {-1 0} {-1 1} {0 -1} {0 1} {1 -1} {1 0} {1 1}}

    foreach dir $directions {
        set newX [expr {$x + [lindex $dir 0]}]
        set newY [expr {$y + [lindex $dir 1]}]
        
        if {$newX >= 0 && $newX < $width && $newY >= 0 && $newY < $height} {
            lset grid $newY $newX [expr {[lindex $grid $newY $newX] + 1}]
            if {[lindex $grid $newY $newX] > 9} {
                incr flashes [flash grid $newX $newY flashed]
            }
        }
    }

    return $flashes
}

# Main program
set grid [readInput input.txt]
set totalFlashes 0

for {set step 0} {$step < 100} {incr step} {
    incr totalFlashes [simulateStep grid]
}

puts $totalFlashes
