set invalidNumber 14360655
set numbers {}

set file [open "input.txt" r]
while {[gets $file line] >= 0} {
    lappend numbers [expr {int($line)}]
}
close $file

set len [llength $numbers]
for {set i 0} {$i < $len} {incr i} {
    set sum [lindex $numbers $i]
    set min $sum
    set max $sum
    for {set j [expr {$i + 1}]} {$j < $len} {incr j} {
        set sum [expr {$sum + [lindex $numbers $j]}]
        set current [lindex $numbers $j]
        if {$current < $min} {set min $current}
        if {$current > $max} {set max $current}
        if {$sum == $invalidNumber} {
            puts [expr {$min + $max}]
            return
        } elseif {$sum > $invalidNumber} {
            break
        }
    }
}