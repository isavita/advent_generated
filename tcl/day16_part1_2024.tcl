
proc solve {} {
    set f [open "input.txt" r]
    set grid [split [read $f] "\n"]
    close $f
    set n [llength $grid]
    set m [string length [lindex $grid 0]]
    for {set i 0} {$i < $n} {incr i} {
        for {set j 0} {$j < $m} {incr j} {
            if {[string index [lindex $grid $i] $j] eq "S"} {
                set sx $i
                set sy $j
            } elseif {[string index [lindex $grid $i] $j] eq "E"} {
                set ex $i
                set ey $j
            }
        }
    }
    set dx {-1 0 1 0}
    set dy {0 1 0 -1}
    set dist [lrepeat $n [lrepeat $m [list 2147483647 2147483647 2147483647 2147483647]]]
    lset dist $sx $sy 1 0
    set h [list [list $sx $sy 1 0]]
    while {[llength $h] > 0} {
        set u [lindex $h 0]
        set h [lrange $h 1 end]
        set x [lindex $u 0]
        set y [lindex $u 1]
        set d [lindex $u 2]
        set cost [lindex $u 3]
        if {[lindex [lindex [lindex $dist $x] $y] $d] < $cost} {
            continue
        }
        if {$x == $ex && $y == $ey} {
            puts $cost
            return
        }
        foreach ndir [list [expr {($d+1)%4}] [expr {($d+3)%4}]] {
            set nc [expr {$cost + 1000}]
            if {$nc < [lindex [lindex [lindex $dist $x] $y] $ndir]} {
                lset dist $x $y $ndir $nc
                lappend h [list $x $y $ndir $nc]
            }
        }
        set nx [expr {$x + [lindex $dx $d]}]
        set ny [expr {$y + [lindex $dy $d]}]
        if {$nx >= 0 && $nx < $n && $ny >= 0 && $ny < $m && [string index [lindex $grid $nx] $ny] ne "#"} {
            set nc [expr {$cost + 1}]
            if {$nc < [lindex [lindex [lindex $dist $nx] $ny] $d]} {
                lset dist $nx $ny $d $nc
                lappend h [list $nx $ny $d $nc]
            }
        }
        set h [lsort -command ::compareNodes $h]
    }
}
proc compareNodes {a b} {
    return [expr {[lindex $a 3] - [lindex $b 3]}]
}
solve
