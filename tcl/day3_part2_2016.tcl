
#!/usr/bin/env tclsh

proc isValidTriangle {a b c} {
    return [expr {$a + $b > $c && $a + $c > $b && $b + $c > $a}]
}

set validTriangles 0
set numbers [list]

set fp [open "input.txt" r]
while {[gets $fp line] >= 0} {
    lappend numbers [regexp -all -inline {\S+} $line]
}
close $fp

set cols [llength [lindex $numbers 0]]
for {set i 0} {$i < $cols} {incr i} {
    for {set j 0} {$j < [llength $numbers]} {set j [expr {$j + 3}]} {
        if {$j + 2 < [llength $numbers]} {
            set a [lindex $numbers $j $i]
            set b [lindex $numbers [expr {$j + 1}] $i]
            set c [lindex $numbers [expr {$j + 2}] $i]
            
            if {[isValidTriangle $a $b $c]} {
                incr validTriangles
            }
        }
    }
}

puts $validTriangles
