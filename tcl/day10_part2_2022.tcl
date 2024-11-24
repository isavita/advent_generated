
proc abs {x} {
    expr {$x < 0 ? -$x : $x}
}

set x {1}
set fp [open "input.txt" r]
while {[gets $fp line] != -1} {
    switch -exact -- $line {
        "noop" {
            lappend x [lindex $x end]
        }
        default {
            regexp {addx (-?\d+)} $line _ n
            lappend x [lindex $x end]
            lappend x [expr {[lindex $x end] + $n}]
        }
    }
}
close $fp

set grid {}
for {set i 0} {$i < [llength $x]} {incr i} {
    set crt_x [expr {$i % 40}]
    set crt_y [expr {$i / 40}]
    if {[abs [expr {$crt_x - [lindex $x $i]}]] <= 1} {
        dict set grid "[list $crt_x $crt_y]" 1
    } else {
        dict unset grid "[list $crt_x $crt_y]"
    }
}

for {set y 0} {$y < 6} {incr y} {
    for {set x 0} {$x < 40} {incr x} {
        if {[dict exists $grid "[list $x $y]"]} {
            puts -nonewline "#"
        } else {
            puts -nonewline "."
        }
    }
    puts ""
}

