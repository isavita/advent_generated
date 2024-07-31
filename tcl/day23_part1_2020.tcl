set file [open "input.txt" r]
set input [read $file]
close $file

array set cups {}
for {set i 0} {$i < [string length $input]} {incr i} {
    set cups($i) 0
}

set currentCup [expr {[string index $input 0] + 0}]
for {set i 0} {$i < [string length $input] - 1} {incr i} {
    set cup [expr {[string index $input $i] + 0}]
    set nextCup [expr {[string index $input [expr {$i + 1}]] + 0}]
    set cups($cup) $nextCup
}
set lastCup [expr {[string index $input end] + 0}]
set cups($lastCup) $currentCup

for {set i 0} {$i < 100} {incr i} {
    set pickup1 $cups($currentCup)
    set pickup2 $cups($pickup1)
    set pickup3 $cups($pickup2)
    set cups($currentCup) $cups($pickup3)

    set destinationCup [expr {$currentCup - 1}]
    if {$destinationCup < 1} { set destinationCup [string length $input] }
    while {$destinationCup == $pickup1 || $destinationCup == $pickup2 || $destinationCup == $pickup3} {
        set destinationCup [expr {$destinationCup - 1}]
        if {$destinationCup < 1} { set destinationCup [string length $input] }
    }

    set cups($pickup3) $cups($destinationCup)
    set cups($destinationCup) $pickup1
    set currentCup $cups($currentCup)
}

set cup $cups(1)
set result ""
while {$cup != 1} {
    append result $cup
    set cup $cups($cup)
}
puts $result