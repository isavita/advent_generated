proc readInput {filename} {
    set file [open $filename r]
    set line [gets $file]
    close $file
    return [split $line " "]
}

proc max {a b} {
    if {$a > $b} {
        return $a
    }
    return $b
}

proc calculateHighScore {players lastMarble} {
    set scores [dict create]
    set circle [list 0]
    set current 0

    for {set marble 1} {$marble <= $lastMarble} {incr marble} {
        if {$marble % 23 == 0} {
            set player [expr {($marble - 1) % $players}]
            dict incr scores $player $marble
            set removeIndex [expr {($current - 7) % [llength $circle]}]
            if {$removeIndex < 0} {set removeIndex [expr {$removeIndex + [llength $circle]}]}
            dict incr scores $player [lindex $circle $removeIndex]
            set circle [lreplace $circle $removeIndex $removeIndex]
            set current $removeIndex
        } else {
            set insertIndex [expr {($current + 2) % [llength $circle]}]
            set circle [linsert $circle $insertIndex $marble]
            set current $insertIndex
        }
    }

    set maxScore 0
    foreach score [dict values $scores] {
        set maxScore [max $maxScore $score]
    }
    return $maxScore
}

set input [readInput "input.txt"]
set players [lindex $input 0]
set lastMarble [lindex $input 6]

puts [calculateHighScore $players $lastMarble]