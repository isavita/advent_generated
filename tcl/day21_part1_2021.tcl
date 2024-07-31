set file [open "input.txt" r]
set data [read $file]
close $file

set lines [split $data "\n"]
set player1Start 0
set player2Start 0

scan [lindex $lines 0] "Player 1 starting position: %d" player1Start
scan [lindex $lines 1] "Player 2 starting position: %d" player2Start

set player1Pos $player1Start
set player2Pos $player2Start
set player1Score 0
set player2Score 0
set dieRoll 1
set rollCount 0

while {1} {
    for {set i 0} {$i < 2} {incr i} {
        set rolls [expr {($dieRoll % 100) + (($dieRoll + 1) % 100) + (($dieRoll + 2) % 100)}]
        set rollCount [expr {$rollCount + 3}]
        set dieRoll [expr {$dieRoll + 3}]
        
        if {$i == 0} {
            set player1Pos [expr {($player1Pos + $rolls - 1) % 10 + 1}]
            set player1Score [expr {$player1Score + $player1Pos}]
            if {$player1Score >= 1000} {
                puts [expr {$player2Score * $rollCount}]
                return
            }
        } else {
            set player2Pos [expr {($player2Pos + $rolls - 1) % 10 + 1}]
            set player2Score [expr {$player2Score + $player2Pos}]
            if {$player2Score >= 1000} {
                puts [expr {$player1Score * $rollCount}]
                return
            }
        }
    }
}