
proc main {} {
    set inputFile "input.txt"
    set f [open $inputFile r]
    set inputLine [gets $f]
    close $f

    set numPlayers ""
    set lastMarble ""
    scan $inputLine "%d players; last marble is worth %d points" numPlayers lastMarble

    set lastMarbleMultiplied [expr {$lastMarble * 100}]

    set max_score [solve_game $numPlayers $lastMarbleMultiplied]

    puts $max_score
}

proc solve_game {numPlayers lastMarble} {
    array set scores {}
    for {set i 0} {$i < $numPlayers} {incr i} {
        set scores($i) 0
    }

    array set nextMarble {}
    array set prevMarble {}

    set currentMarble 0
    set nextMarble(0) 0
    set prevMarble(0) 0

    set currentPlayer 0

    for {set marble 1} {$marble <= $lastMarble} {incr marble} {
        if {$marble % 23 == 0} {
            set scores($currentPlayer) [expr {$scores($currentPlayer) + $marble}]

            set marbleToRemove $currentMarble
            for {set i 0} {$i < 7} {incr i} {
                set marbleToRemove $prevMarble($marbleToRemove)
            }

            set scores($currentPlayer) [expr {$scores($currentPlayer) + $marbleToRemove}]

            set marbleBefore [set prevMarble($marbleToRemove)]
            set marbleAfter [set nextMarble($marbleToRemove)]

            set nextMarble($marbleBefore) $marbleAfter
            set prevMarble($marbleAfter) $marbleBefore

            set currentMarble $marbleAfter

        } else {
            set pos1 [set nextMarble($currentMarble)]
            set pos2 [set nextMarble($pos1)]

            set nextMarble($marble) $pos2
            set prevMarble($marble) $pos1

            set nextMarble($pos1) $marble
            set prevMarble($pos2) $marble

            set currentMarble $marble
        }

        set currentPlayer [expr {($currentPlayer + 1) % $numPlayers}]
    }

    set max_score 0
    foreach player [array names scores] {
        if {$scores($player) > $max_score} {
            set max_score $scores($player)
        }
    }

    return $max_score
}

main
