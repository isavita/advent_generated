proc readDeck {file} {
    set player1 {}
    set player2 {}
    set currentDeck player1
    set f [open $file r]
    while {[gets $f line] >= 0} {
        if {$line eq ""} {
            set currentDeck player2
            continue
        }
        if {[string match "Player*" $line]} {
            continue
        }
        lappend $currentDeck [scan $line "%d"]
    }
    close $f
    return [list $player1 $player2]
}

proc copyDeck {deck n} {
    return [lrange $deck 0 [expr {$n - 1}]]
}

proc scoreDeck {deck} {
    set score 0
    set len [llength $deck]
    for {set i 0} {$i < $len} {incr i} {
        set card [lindex $deck $i]
        set score [expr {$score + $card * ($len - $i)}]
    }
    return $score
}

proc playRecursiveCombat {player1 player2} {
    array set previousRounds {}
    while {[llength $player1] > 0 && [llength $player2] > 0} {
        set roundKey [join [list $player1 $player2] "|"]
        if {[info exists previousRounds($roundKey)]} {
            return [list $player1 {}]
        }
        set previousRounds($roundKey) 1

        set card1 [lindex $player1 0]
        set card2 [lindex $player2 0]
        set player1 [lrange $player1 1 end]
        set player2 [lrange $player2 1 end]

        if {[llength $player1] >= $card1 && [llength $player2] >= $card2} {
            set subDecks [playRecursiveCombat [copyDeck $player1 $card1] [copyDeck $player2 $card2]]
            set subPlayer1 [lindex $subDecks 0]
            set subPlayer2 [lindex $subDecks 1]
            if {[llength $subPlayer1] > 0} {
                lappend player1 $card1 $card2
            } else {
                lappend player2 $card2 $card1
            }
        } else {
            if {$card1 > $card2} {
                lappend player1 $card1 $card2
            } else {
                lappend player2 $card2 $card1
            }
        }
    }
    return [list $player1 $player2]
}

set decks [readDeck "input.txt"]
set player1 [lindex $decks 0]
set player2 [lindex $decks 1]
set results [playRecursiveCombat $player1 $player2]
set winner [lindex $results 0]
set loser [lindex $results 1]
set winningDeck [expr {[llength $winner] > 0 ? $winner : $loser}]
puts [scoreDeck $winningDeck]