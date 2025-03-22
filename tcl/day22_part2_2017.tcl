
proc solve {filename} {
    set grid {}
    set startX 0
    set startY 0
    set y -1

    set inputFile [open $filename r]
    while {[gets $inputFile line] >= 0} {
        incr y
        set x -1
        foreach c [split $line ""] {
            incr x
            if {$c eq "#"} {
                dict set grid "$x $y" 2
            }
        }
        set startX [expr {[string length $line] / 2}]
        set startY [expr {$y / 2}]
    }
    close $inputFile

    set dx {0 1 0 -1}
    set dy {-1 0 1 0}

    set x $startX
    set y $startY
    set dir 0
    set infectedCount 0

    for {set i 0} {$i < 10000000} {incr i} {
        set pos "$x $y"
        if {![dict exists $grid $pos]} {
            dict set grid $pos 0
        }

        set state [dict get $grid $pos]
        if {$state == 0} {
            set dir [expr {($dir - 1 + 4) % 4}]
            dict set grid $pos 1
        } elseif {$state == 1} {
            dict set grid $pos 2
            incr infectedCount
        } elseif {$state == 2} {
            set dir [expr {($dir + 1) % 4}]
            dict set grid $pos 3
        } else {
            set dir [expr {($dir + 2) % 4}]
            dict set grid $pos 0
        }

        set x [expr {$x + [lindex $dx $dir]}]
        set y [expr {$y + [lindex $dy $dir]}]
    }

    puts $infectedCount
}

solve "input.txt"
