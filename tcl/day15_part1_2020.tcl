
#!/usr/bin/env tclsh

set fp [open "input.txt" r]
set input [string trim [read $fp]]
close $fp

set startingNumbers [split $input ","]
set lastSpoken [dict create]

set lastNumber 0
set turn 1

foreach num $startingNumbers {
    dict set lastSpoken $num $turn
    set lastNumber $num
    incr turn
}

while {$turn <= 2020} {
    if {[dict exists $lastSpoken $lastNumber]} {
        set lastTurn [dict get $lastSpoken $lastNumber]
        set nextNumber [expr {$turn - 1 - $lastTurn}]
    } else {
        set nextNumber 0
    }
    
    dict set lastSpoken $lastNumber [expr {$turn - 1}]
    set lastNumber $nextNumber
    incr turn
}

puts $lastNumber
