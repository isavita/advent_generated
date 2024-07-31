set file [open "input.txt" r]
set input [read $file]
close $file

set scoreboard {3 7}
set elf1 0
set elf2 1
set inputLen [string length $input]
set inputSequence {}

for {set i 0} {$i < $inputLen} {incr i} {
    lappend inputSequence [expr {[string index $input $i] + 0}]
}

proc checkSequence {scoreboard sequence} {
    set lenScoreboard [llength $scoreboard]
    set lenSequence [llength $sequence]
    if {$lenScoreboard < $lenSequence} {return 0}
    set start [expr {$lenScoreboard - $lenSequence}]
    for {set i 0} {$i < $lenSequence} {incr i} {
        if {[lindex $scoreboard [expr {$start + $i}]] != [lindex $sequence $i]} {
            return 0
        }
    }
    return 1
}

while {1} {
    set newScore [expr {[lindex $scoreboard $elf1] + [lindex $scoreboard $elf2]}]
    if {$newScore >= 10} {
        lappend scoreboard [expr {$newScore / 10}]
        if {[checkSequence $scoreboard $inputSequence]} {break}
    }
    lappend scoreboard [expr {$newScore % 10}]
    if {[checkSequence $scoreboard $inputSequence]} {break}

    set elf1 [expr {($elf1 + [lindex $scoreboard $elf1] + 1) % [llength $scoreboard]}]
    set elf2 [expr {($elf2 + [lindex $scoreboard $elf2] + 1) % [llength $scoreboard]}]
}

puts [expr {[llength $scoreboard] - $inputLen}]