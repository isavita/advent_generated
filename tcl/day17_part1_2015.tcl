proc countCombinations {containers target index} {
    if {$target == 0} {
        return 1
    }
    if {$target < 0 || $index >= [llength $containers]} {
        return 0
    }
    set include [countCombinations $containers [expr {$target - [lindex $containers $index]}] [expr {$index + 1}]]
    set exclude [countCombinations $containers $target [expr {$index + 1}]]
    return [expr {$include + $exclude}]
}

set file [open "input.txt" r]
set containers {}
while {[gets $file line] >= 0} {
    lappend containers $line
}
close $file

puts [countCombinations $containers 150 0]