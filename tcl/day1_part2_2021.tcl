set file [open "input.txt" r]
set vals {}
while {[gets $file line] >= 0} {
    if {[string length $line] > 0} {
        lappend vals [expr {$line}]
    }
}
close $file

set prevSum [expr {[lindex $vals 0] + [lindex $vals 1] + [lindex $vals 2]}]
set count 0

for {set i 3} {$i < [llength $vals]} {incr i} {
    set currSum [expr {[lindex $vals [expr {$i-2}]] + [lindex $vals [expr {$i-1}]] + [lindex $vals $i]}]
    if {$currSum > $prevSum} {
        incr count
    }
    set prevSum $currSum
}

puts $count