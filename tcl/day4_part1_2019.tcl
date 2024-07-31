set file [open "input.txt" r]
set rangeStr [gets $file]
close $file

set parts [split $rangeStr "-"]
set start [lindex $parts 0]
set end [lindex $parts 1]

proc hasDoubleAndIncreasingDigits {s} {
    set hasDouble 0
    for {set i 0} {$i < [string length $s] - 1} {incr i} {
        if {[string index $s $i] == [string index $s [expr {$i + 1}]]} {
            set hasDouble 1
        }
        if {[string index $s $i] > [string index $s [expr {$i + 1}]]} {
            return 0
        }
    }
    return $hasDouble
}

set count 0
for {set i $start} {$i <= $end} {incr i} {
    set s [format "%d" $i]
    if {[hasDoubleAndIncreasingDigits $s]} {
        incr count
    }
}

puts $count