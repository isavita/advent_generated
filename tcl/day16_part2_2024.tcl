
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
        set ux [lindex $u 0]
        set uy [lindex $u 1]
        set ud [lindex $u 2]
        set cost [lindex $u 3]
        if {[lindex [lindex [lindex $dist $ux] $uy] $ud] < $cost} {
            continue
        }
        if {$ux == $ex && $uy == $ey} {
            continue
        }
        foreach ndir [list [expr {($ud+1)%4}] [expr {($ud+3)%4}]] {
            set nc [expr {$cost + 1000}]
            if {$nc < [lindex [lindex [lindex $dist $ux] $uy] $ndir]} {
                lset dist $ux $uy $ndir $nc
                lappend h [list $ux $uy $ndir $nc]
            }
        }
        set nx [expr {$ux + [lindex $dx $ud]}]
        set ny [expr {$uy + [lindex $dy $ud]}]
        if {$nx >= 0 && $nx < $n && $ny >= 0 && $ny < $m && [string index [lindex $grid $nx] $ny] ne "#"} {
            set nc [expr {$cost + 1}]
            if {$nc < [lindex [lindex [lindex $dist $nx] $ny] $ud]} {
                lset dist $nx $ny $ud $nc
                lappend h [list $nx $ny $ud $nc]
            }
        }
        set h [lsort -index 3 -increasing $h]
    }
    set best 2147483647
    for {set d 0} {$d < 4} {incr d} {
        if {[lindex [lindex [lindex $dist $ex] $ey] $d] < $best} {
            set best [lindex [lindex [lindex $dist $ex] $ey] $d]
        }
    }
    set used [lrepeat $n [lrepeat $m 0]]
    set rev {}
    for {set d 0} {$d < 4} {incr d} {
        if {[lindex [lindex [lindex $dist $ex] $ey] $d] == $best} {
            lappend rev [list $ex $ey $d]
        }
    }
    set vis [lrepeat $n [lrepeat $m [list 0 0 0 0]]]
    foreach s $rev {
        lset vis [lindex $s 0] [lindex $s 1] [lindex $s 2] 1
    }
    while {[llength $rev] > 0} {
        set u [lindex $rev end]
        set rev [lrange $rev 0 end-1]
        set ux [lindex $u 0]
        set uy [lindex $u 1]
        set ud [lindex $u 2]
        lset used $ux $uy 1
        set costU [lindex [lindex [lindex $dist $ux] $uy] $ud]
        foreach pd [list [expr {($ud+1)%4}] [expr {($ud+3)%4}]] {
            if {[lindex [lindex [lindex $dist $ux] $uy] $pd] == [expr {$costU - 1000}]} {
                if {![lindex [lindex [lindex $vis $ux] $uy] $pd]} {
                    lset vis $ux $uy $pd 1
                    lappend rev [list $ux $uy $pd]
                }
            }
        }
        set px [expr {$ux - [lindex $dx $ud]}]
        set py [expr {$uy - [lindex $dy $ud]}]
        if {$px >= 0 && $px < $n && $py >= 0 && $py < $m && [string index [lindex $grid $px] $py] ne "#"} {
            if {[lindex [lindex [lindex $dist $px] $py] $ud] == [expr {$costU - 1}]} {
                if {![lindex [lindex [lindex $vis $px] $py] $ud]} {
                    lset vis $px $py $ud 1
                    lappend rev [list $px $py $ud]
                }
            }
        }
    }
    set cnt 0
    for {set i 0} {$i < $n} {incr i} {
        for {set j 0} {$j < $m} {incr j} {
            if {[lindex [lindex $used $i] $j] && [string index [lindex $grid $i] $j] ne "#"} {
                incr cnt
            }
        }
    }
    puts $cnt
}
solve
