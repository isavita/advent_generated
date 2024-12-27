
proc canReach {grid} {
    set n [llength $grid]
    if {[lindex [lindex $grid 0] 0] || [lindex [lindex $grid end] end]} {
        return 0
    }
    set dirs {{1 0} {-1 0} {0 1} {0 -1}}
    set visited {}
    for {set i 0} {$i < $n} {incr i} {
        lappend visited [lrepeat $n 0]
    }
    set q {{0 0}}
    lset visited 0 0 1
    while {[llength $q] > 0} {
        set c [lindex $q 0]
        set q [lrange $q 1 end]
        if {[lindex $c 0] == $n-1 && [lindex $c 1] == $n-1} {
            return 1
        }
        foreach d $dirs {
            set nx [expr {[lindex $c 0] + [lindex $d 0]}]
            set ny [expr {[lindex $c 1] + [lindex $d 1]}]
            if {$nx >= 0 && $ny >= 0 && $nx < $n && $ny < $n && !([lindex [lindex $grid $ny] $nx]) && !([lindex [lindex $visited $ny] $nx])} {
                lset visited $ny $nx 1
                lappend q [list $nx $ny]
            }
        }
    }
    return 0
}

set f [open "input.txt" r]
set size 71
set grid {}
for {set i 0} {$i < $size} {incr i} {
    lappend grid [lrepeat $size 0]
}
while {[gets $f line] != -1} {
    set parts [split $line ","]
    set x [lindex $parts 0]
    set y [lindex $parts 1]
    if {$x >= 0 && $x < $size && $y >= 0 && $y < $size} {
        lset grid $y $x 1
    }
    if {![canReach $grid]} {
        puts "$x,$y"
        close $f
        return
    }
}
puts "No cutoff found"
close $f
