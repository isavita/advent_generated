proc readInput {filename} {
    set histories {}
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        lappend histories [split $line]
    }
    close $file
    return $histories
}

proc calculateDifferences {sequence} {
    set diffs {}
    for {set i 0} {$i < [llength $sequence] - 1} {incr i} {
        lappend diffs [expr {[lindex $sequence [expr {$i + 1}]] - [lindex $sequence $i]}]
    }
    return $diffs
}

proc extrapolatePreviousValue {history} {
    set differences [list $history]
    while {1} {
        set lastDiffs [lindex $differences end]
        set newDiffs [calculateDifferences $lastDiffs]
        if {[llength $newDiffs] == 0 || [llength [lsearch -all $newDiffs 0]] == [llength $newDiffs]} {
            break
        }
        lappend differences $newDiffs
    }

    set zeroes [lindex $differences end]
    lappend zeroes 0

    for {set i [expr {[llength $differences] - 2}]} {$i >= 0} {incr i -1} {
        set prevDiffs [lindex $differences $i]
        set newFirst [expr {[lindex $prevDiffs 0] - [lindex $zeroes 0]}]
        set zeroes [linsert $zeroes 0 $newFirst]
    }

    return [lindex $zeroes 0]
}

set histories [readInput "input.txt"]
set sum 0
foreach history $histories {
    set prevValue [extrapolatePreviousValue $history]
    set sum [expr {$sum + $prevValue}]
}
puts $sum