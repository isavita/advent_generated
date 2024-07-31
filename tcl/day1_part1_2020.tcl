set file [open "input.txt" r]
set numbers [list]

while {[gets $file line] >= 0} {
    if {$line ne ""} {
        lappend numbers [scan $line "%d"]
    }
}
close $file

set found 0
for {set i 0} {$i < [llength $numbers] - 1} {incr i} {
    for {set j [expr {$i + 1}]} {$j < [llength $numbers]} {incr j} {
        if {[expr {[lindex $numbers $i] + [lindex $numbers $j]}] == 2020} {
            set found 1
            puts [expr {[lindex $numbers $i] * [lindex $numbers $j]}]
            break
        }
    }
    if {$found} {break}
}