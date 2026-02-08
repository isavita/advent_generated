
set f [open input.txt r]
set depth [scan [gets $f] "depth: %d"]
lassign [regexp -all -inline {\d+} [gets $f]] tx ty
close $f

set maxX [expr {$tx + 100}]
set maxY [expr {$ty + 100}]
set erosions [lrepeat [expr {$maxY + 1}] 0]
for {set y 0} {$y <= $maxY} {incr y} {
    set row [lrepeat [expr {$maxX + 1}] 0]
    for {set x 0} {$x <= $maxX} {incr x} {
        if {$x == 0 && $y == 0} {
            set gi 0
        } elseif {$x == $tx && $y == $ty} {
            set gi 0
        } elseif {$y == 0} {
            set gi [expr {$x * 16807}]
        } elseif {$x == 0} {
            set gi [expr {$y * 48271}]
        } else {
            set gi [expr {[lindex $row [expr {$x - 1}]] * [lindex $erosions [expr {$y - 1}] $x]}]
        }
        set el [expr {($gi + $depth) % 20183}]
        lset row $x $el
    }
    lset erosions $y $row
}

set risk 0
for {set y 0} {$y <= $ty} {incr y} {
    for {set x 0} {$x <= $tx} {incr x} {
        incr risk [expr {[lindex $erosions $y $x] % 3}]
    }
}
puts $risk

array set visited {}
set queue [list [list 0 0 torch 0]]
while {[llength $queue]} {
    set queue [lsort -integer -index 3 $queue]
    lassign [lindex $queue 0] x y tool mins
    set queue [lrange $queue 1 end]
    set key "$x,$y,$tool"
    if {[info exists visited($key)]} continue
    set visited($key) 1
    if {$x == $tx && $y == $ty && $tool eq "torch"} {
        puts $mins
        exit
    }
    set r [expr {[lindex $erosions $y $x] % 3}]
    set tools [lindex { {torch climbing} {climbing neither} {torch neither}} $r]
    foreach t $tools {
        if {$t ne $tool} {
            lappend queue [list $x $y $t [expr {$mins + 7}]]
        }
    }
    foreach {dx dy} {0 1 1 0 0 -1 -1 0} {
        set nx [expr {$x + $dx}]
        set ny [expr {$y + $dy}]
        if {$nx < 0 || $ny < 0 || $nx > $maxX || $ny > $maxY} continue
        set nr [expr {[lindex $erosions $ny $nx] % 3}]
        set nt [lindex { {torch climbing} {climbing neither} {torch neither}} $nr]
        if {$tool in $nt} {
            lappend queue [list $nx $ny $tool [expr {$mins + 1}]]
        }
    }
}
