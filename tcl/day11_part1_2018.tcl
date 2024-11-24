
set serial [string trim [read [open "input.txt" r]]]
set gridSize 300
set grid [lrepeat $gridSize [lrepeat $gridSize 0]]

for {set y 0} {$y < $gridSize} {incr y} {
    for {set x 0} {$x < $gridSize} {incr x} {
        set rackID [expr {$x + 11}]
        set powerLevel [expr {($rackID * ($y + 1) + $serial) * $rackID / 100 % 10 - 5}]
        lset grid $y $x $powerLevel
    }
}

set maxPower -2147483648 ;# Minimum 32-bit signed integer
set maxX 0
set maxY 0

for {set y 0} {$y < [expr {$gridSize - 2}]} {incr y} {
    for {set x 0} {$x < [expr {$gridSize - 2}]} {incr x} {
        set totalPower 0
        for {set dy 0} {$dy < 3} {incr dy} {
            for {set dx 0} {$dx < 3} {incr dx} {
                set totalPower [expr {$totalPower + [lindex [lindex $grid [expr {$y + $dy}]] [expr {$x + $dx}]]}]
            }
        }
        if {$totalPower > $maxPower} {
            set maxPower $totalPower
            set maxX [expr {$x + 1}]
            set maxY [expr {$y + 1}]
        }
    }
}

puts "[expr {$maxX}],[expr {$maxY}]"
