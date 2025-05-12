
proc parseNumbers {numString} {
    set numbers {}
    foreach num [regexp -all -inline {\d+} $numString] {
        lappend numbers $num
    }
    return $numbers
}

proc calculatePoints {winningNumbers yourNumbers} {
    array set winningDict {}
    foreach num $winningNumbers {
        set winningDict($num) 1
    }

    set points 0
    foreach num $yourNumbers {
        if {[info exists winningDict($num)]} {
            if {$points == 0} {
                set points 1
            } else {
                set points [expr {$points * 2}]
            }
        }
    }
    unset winningDict
    return $points
}

set totalPoints 0
set fileId [open "input.txt" r]

while {[gets $fileId line] >= 0} {
    set parts [split $line ":"]
    set numString [lindex $parts 1]

    set numParts [split $numString "|"]
    set winningStr [string trim [lindex $numParts 0]]
    set yourStr    [string trim [lindex $numParts 1]]

    set winningNums [parseNumbers $winningStr]
    set yourNums    [parseNumbers $yourStr]

    set cardPoints [calculatePoints $winningNums $yourNums]
    set totalPoints [expr {$totalPoints + $cardPoints}]
}

close $fileId

puts $totalPoints
