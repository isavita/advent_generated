proc isValidPassword {password} {
    set s [split $password ""]
    set hasDouble 0

    for {set i 0} {$i < [llength $s] - 1} {incr i} {
        if {[lindex $s $i] > [lindex $s [expr {$i + 1}]]} {
            return 0
        }
        if {[lindex $s $i] == [lindex $s [expr {$i + 1}]]} {
            if {($i == 0 || [lindex $s $i] != [lindex $s [expr {$i - 1}]] ) && 
                ([expr {$i + 2}] >= [llength $s] || [lindex $s $i] != [lindex $s [expr {$i + 2}]] )} {
                set hasDouble 1
            }
        }
    }
    return $hasDouble
}

set file [open "input.txt" r]
set data [read $file]
close $file

set range [split [string trim $data] "-"]
set start [lindex $range 0]
set end [lindex $range 1]

set count 0
for {set i $start} {$i <= $end} {incr i} {
    if {[isValidPassword $i]} {
        incr count
    }
}

puts $count