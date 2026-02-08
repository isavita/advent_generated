#!/usr/bin/env tclsh

# Read points from input.txt
set xs {}
set ys {}
set fp [open "input.txt" r]
while {[gets $fp line] >= 0} {
    set line [string trim $line]
    if {$line eq ""} continue
    set parts [split $line ,]
    lappend xs [lindex $parts 0]
    lappend ys [lindex $parts 1]
}
close $fp

set n [llength $xs]
set best 0

for {set i 0} {$i < $n} {incr i} {
    set x1 [lindex $xs $i]
    set y1 [lindex $ys $i]
    for {set j $i} {$j < $n} {incr j} {
        set dx [expr {abs($x1 - [lindex $xs $j]) + 1}]
        set dy [expr {abs($y1 - [lindex $ys $j]) + 1}]
        set area [expr {$dx * $dy}]
        if {$area > $best} {set best $area}
    }
}

puts $best