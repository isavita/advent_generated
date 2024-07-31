set file [open "input.txt" r]
set count 0

proc parseRange {r} {
    set parts [split $r "-"]
    return [list [lindex $parts 0] [lindex $parts 1]]
}

while {[gets $file line] >= 0} {
    set ranges [split $line ","]
    if {[llength $ranges] != 2} continue

    set range1 [parseRange [lindex $ranges 0]]
    set range2 [parseRange [lindex $ranges 1]]
    
    set start1 [lindex $range1 0]
    set end1 [lindex $range1 1]
    set start2 [lindex $range2 0]
    set end2 [lindex $range2 1]

    if {($start1 <= $start2 && $end1 >= $end2) || ($start2 <= $start1 && $end2 >= $end1)} {
        incr count
    }
}

close $file
puts $count