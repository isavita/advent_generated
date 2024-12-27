
set f [open "input.txt" r]
set line [read $f]
close $f
set fishes [lrepeat 9 0]
foreach fish [split $line ","] {
    lset fishes $fish [expr {[lindex $fishes $fish] + 1}]
}
for {set day 1} {$day <= 80} {incr day} {
    set newFish [lindex $fishes 0]
    for {set i 1} {$i < 9} {incr i} {
        lset fishes [expr {$i - 1}] [lindex $fishes $i]
    }
    lset fishes 6 [expr {[lindex $fishes 6] + $newFish}]
    lset fishes 8 $newFish
}
set totalFish 0
foreach fish $fishes {
    set totalFish [expr {$totalFish + $fish}]
}
puts $totalFish
