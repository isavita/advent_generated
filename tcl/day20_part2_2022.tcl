
proc mix {nums} {
    set n [expr {[llength $nums] - 1}]
    for {set i 0} {$i < [llength $nums]} {incr i} {
        set oldpos [lindex [lindex $nums $i] 0]
        set newpos [expr {(([lindex [lindex $nums $i] 0] + [lindex [lindex $nums $i] 1]) % $n + $n) % $n}]

        if {$oldpos < $newpos} {
            for {set j 0} {$j < [llength $nums]} {incr j} {
                if {([lindex [lindex $nums $j] 0] > $oldpos) && ([lindex [lindex $nums $j] 0] <= $newpos)} {
                    lset nums $j 0 [expr {[lindex [lindex $nums $j] 0] - 1}]
                }
            }
        } elseif {$newpos < $oldpos} {
            for {set j 0} {$j < [llength $nums]} {incr j} {
                if {([lindex [lindex $nums $j] 0] >= $newpos) && ([lindex [lindex $nums $j] 0] < $oldpos)} {
                    lset nums $j 0 [expr {[lindex [lindex $nums $j] 0] + 1}]
                }
            }
        }
        lset nums $i 0 $newpos
    }
    return $nums
}

proc coords {nums} {
    set l [llength $nums]
    set zero_pos ""
    for {set i 0} {$i < [llength $nums]} {incr i} {
        if {[lindex [lindex $nums $i] 1] == 0} {
            set zero_pos [lindex [lindex $nums $i] 0]
            break
        }
    }
    set total_sum 0
    for {set i 0} {$i < [llength $nums]} {incr i} {
        set pos [lindex [lindex $nums $i] 0]
        if {($pos == [expr {($zero_pos + 1000) % $l}]) || ($pos == [expr {($zero_pos + 2000) % $l}]) || ($pos == [expr {($zero_pos + 3000) % $l}])} {
            incr total_sum [lindex [lindex $nums $i] 1]
        }
    }
    return $total_sum
}

proc main {} {
    set f [open "input.txt" r]
    set lines [split [read $f] "\n"]
    close $f

    set nums {}
    set i 0
    foreach line $lines {
        lappend nums [list $i [string trim $line]]
        incr i
    }
    
    set nums2 {}
    for {set i 0} {$i < [llength $nums]} {incr i} {
    lappend nums2 [list [lindex [lindex $nums $i] 0] [expr {811589153 * [lindex [lindex $nums $i] 1]}]]
}

    for {set i 0} {$i < 10} {incr i} {
        set nums2 [mix $nums2]
    }
    puts [coords $nums2]
}

main
