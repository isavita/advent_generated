set file [open "input.txt" r]
set count 0

proc parseRange {s} {
    set split [split $s "-"]
    return [list [lindex $split 0] [lindex $split 1]]
}

while {[gets $file line] >= 0} {
    set pair [split $line ","]
    set left [parseRange [lindex $pair 0]]
    set right [parseRange [lindex $pair 1]]

    if {[lindex $left 0] <= [lindex $right 1] && [lindex $left 1] >= [lindex $right 0]} {
        incr count
    }
}

close $file
puts $count