
set f [open "input.txt"]
set grid {}
while {[gets $f line] >= 0} {
    lappend grid $line
}
close $f
set height [llength $grid]
set width [string length [lindex $grid 0]]
set sx 0
set sy 0
for {set y 0} {$y < $height} {incr y} {
    set line [lindex $grid $y]
    set pos [string first S $line]
    if {$pos != -1} {
        set sx $pos
        set sy $y
        break
    }
}
set splits 0
set active [list]
for {set i 0} {$i < $width} {incr i} {lappend active 0}
lset active $sx 1
for {set y $sy} {$y < $height} {incr y} {
    set next [list]
    for {set i 0} {$i < $width} {incr i} {lappend next 0}
    set line [lindex $grid $y]
    for {set x 0} {$x < $width} {incr x} {
        if {[lindex $active $x] == 0} continue
        if {[string index $line $x] eq "^"} {
            incr splits
            if {$x > 0} {lset next [expr {$x-1}] 1}
            if {$x < $width-1} {lset next [expr {$x+1}] 1}
        } else {
            lset next $x 1
        }
    }
    set active $next
    if {[lsearch -exact $active 1] == -1} break
}
puts $splits
