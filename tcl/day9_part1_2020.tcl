set preambleLength 25
set file [open "input.txt" r]
set numbers [list]

while {[gets $file line] >= 0} {
    lappend numbers [expr {$line}]
}
close $file

for {set i $preambleLength} {$i < [llength $numbers]} {incr i} {
    set valid 0
    set target [lindex $numbers $i]
    set seen [dict create]

    for {set j [expr {$i - $preambleLength}]} {$j < $i} {incr j} {
        set num [lindex $numbers $j]
        if {[dict exists $seen [expr {$target - $num}]]} {
            set valid 1
            break
        }
        dict set seen $num 1
    }
    
    if {!$valid} {
        puts $target
        break
    }
}