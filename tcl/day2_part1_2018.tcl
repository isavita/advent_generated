set file [open "input.txt" r]
set twoCount 0
set threeCount 0

proc countTwosAndThrees {id} {
    set charCount [dict create]
    foreach char [split $id ""] {
        dict incr charCount $char
    }
    set hasTwos 0
    set hasThrees 0
    foreach count [dict values $charCount] {
        if {$count == 2} {set hasTwos 1}
        if {$count == 3} {set hasThrees 1}
    }
    return [list $hasTwos $hasThrees]
}

while {[gets $file line] >= 0} {
    set counts [countTwosAndThrees $line]
    set twos [lindex $counts 0]
    set threes [lindex $counts 1]
    if {$twos} {incr twoCount}
    if {$threes} {incr threeCount}
}

close $file
puts [expr {$twoCount * $threeCount}]