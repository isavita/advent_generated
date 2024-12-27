
proc evenDigits s {
    expr {[string length $s] % 2 == 0}
}
proc trimLeadingZeros s {
    while {[string length $s] > 1 && [string index $s 0] eq "0"} {
        set s [string range $s 1 end]
    }
    return $s
}
set f [open "input.txt" r]
set line [string trim [read $f]]
close $f
set stones [split $line]
for {set i 0} {$i < 25} {incr i} {
    set next {}
    foreach s $stones {
        if {$s eq "0"} {
            lappend next 1
        } elseif {[evenDigits $s]} {
            set mid [expr {[string length $s] / 2}]
            set left [trimLeadingZeros [string range $s 0 [expr {$mid - 1}]]]
            set right [trimLeadingZeros [string range $s $mid end]]
            if {$left eq ""} {set left 0}
            if {$right eq ""} {set right 0}
            lappend next $left $right
        } else {
            lappend next [expr {$s * 2024}]
        }
    }
    set stones $next
}
puts [llength $stones]
