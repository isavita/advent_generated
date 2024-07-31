set player1Deck {}
set player2Deck {}
set currentDeck player1Deck

set file [open "input.txt" r]
while {[gets $file line] >= 0} {
    if {$line eq ""} {
        set currentDeck player2Deck
        continue
    }
    if {[string first "Player" $line] != -1} {
        continue
    }
    lappend $currentDeck [scan $line "%d"]
}
close $file

while {[llength $player1Deck] > 0 && [llength $player2Deck] > 0} {
    set card1 [lindex $player1Deck 0]
    set card2 [lindex $player2Deck 0]
    set player1Deck [lrange $player1Deck 1 end]
    set player2Deck [lrange $player2Deck 1 end]
    if {$card1 > $card2} {
        lappend player1Deck $card1 $card2
    } else {
        lappend player2Deck $card2 $card1
    }
}

set winningDeck [expr {[llength $player1Deck] > 0 ? $player1Deck : $player2Deck}]
set score 0
set len [llength $winningDeck]
for {set i 0} {$i < $len} {incr i} {
    set card [lindex $winningDeck $i]
    set score [expr {$score + $card * ($len - $i)}]
}
puts $score