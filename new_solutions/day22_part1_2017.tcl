proc readInput {filename} {
    set file [open $filename r]
    set grid [dict create]
    set y 0
    while {[gets $file line] >= 0} {
        set x 0
        foreach char [split $line ""] {
            if {$char eq "#"} {
                dict set grid "$x,$y" 1
            }
            incr x
        }
        incr y
    }
    close $file
    return [list $grid [expr {$x / 2}] [expr {$y / 2}]]
}

proc simulateVirus {grid x y bursts} {
    set direction 0
    set infections 0
    set directions {{0 -1} {1 0} {0 1} {-1 0}}

    for {set i 0} {$i < $bursts} {incr i} {
        set key "$x,$y"
        if {[dict exists $grid $key]} {
            set direction [expr {($direction + 1) % 4}]
            dict unset grid $key
        } else {
            set direction [expr {($direction + 3) % 4}]
            dict set grid $key 1
            incr infections
        }
        set dx [lindex [lindex $directions $direction] 0]
        set dy [lindex [lindex $directions $direction] 1]
        set x [expr {$x + $dx}]
        set y [expr {$y + $dy}]
    }
    return $infections
}

set data [readInput "input.txt"]
set grid [lindex $data 0]
set startX [lindex $data 1]
set startY [lindex $data 2]
set result [simulateVirus $grid $startX $startY 10000]
puts $result