#!/usr/bin/env tclsh
proc calc {s} {
    set len [string length $s]
    for {set d1 9} {$d1 >= 0} {incr d1 -1} {
        set pos [string first $d1 $s]
        if {$pos == -1 || $pos == $len-1} continue
        set tail [string range $s [expr {$pos+1}] end]
        set digits [regexp -all -inline {\d} $tail]
        if {[llength $digits]} {
            set max2 [lindex [lsort -integer -decreasing $digits] 0]
            return [expr {$d1*10+$max2}]
        }
    }
    return 0
}
set total 0
set f [open "input.txt"]
while {[gets $f line] >= 0} {
    set total [expr {$total + [calc $line]}]
}
close $f
puts $total