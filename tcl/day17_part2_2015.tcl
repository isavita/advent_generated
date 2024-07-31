proc findCombinations {containers target index count minCount ways} {
    if {$target == 0} {
        if {$minCount == 0 || $count < $minCount} {
            set minCount $count
            set ways 1
        } elseif {$count == $minCount} {
            incr ways
        }
        return [list $minCount $ways]
    }
    if {$target < 0 || $index >= [llength $containers]} {
        return [list $minCount $ways]
    }
    set result [findCombinations $containers [expr {$target - [lindex $containers $index]}] [expr {$index + 1}] [expr {$count + 1}] $minCount $ways]
    set minCount [lindex $result 0]
    set ways [lindex $result 1]
    set result [findCombinations $containers $target [expr {$index + 1}] $count $minCount $ways]
    return $result
}

set file [open "input.txt" r]
set containers {}
while {[gets $file line] >= 0} {
    lappend containers [expr {int($line)}]
}
close $file

set minCount 0
set ways 0
set result [findCombinations $containers 150 0 0 $minCount $ways]
puts [lindex $result 1]