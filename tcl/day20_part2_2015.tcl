
set target [string trim [read [open input.txt r]]]
set target [expr {$target / 11}]

set houses [lrepeat [expr {$target + 1}] 0]

for {set elf 1} {$elf <= $target} {incr elf} {
    for {set house $elf} {$house <= [expr {$elf * 50}] && $house <= $target} {incr house $elf} {
        lset houses $house [expr {[lindex $houses $house] + $elf}]
    }
}

for {set i 0} {$i <= $target} {incr i} {
    if {[lindex $houses $i] >= $target} {
        puts $i
        break
    }
}
