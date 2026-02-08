#!/usr/bin/tclsh
set target 12
set sum 0
set fp [open "input.txt" r]
while {[gets $fp line] >= 0} {
    regsub {[^0-9]+$} $line {} line
    set len [string length $line]
    if {$len < $target} continue
    set rem [expr {$len - $target}]
    set stack {}
    for {set i 0} {$i < $len} {incr i} {
        set c [string index $line $i]
        while {$rem > 0 && [llength $stack] > 0 && [lindex $stack end] < $c} {
            set stack [lrange $stack 0 end-1]
            set rem [expr {$rem - 1}]
        }
        lappend stack $c
    }
    set numStr [join [lrange $stack 0 [expr {$target - 1}]] ""]
    set sum [expr {$sum + $numStr}]
}
close $fp
puts $sum