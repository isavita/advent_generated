set file [open "input.txt" r]
set data [read $file]
close $file

set target [string trim $data]
set target [expr {$target + 0}]

set sideLength [expr {ceil(sqrt($target))}]
set sideLength [expr {int($sideLength)}]  ;# Convert to integer

if {$sideLength % 2 == 0} {
    set sideLength [expr {$sideLength + 1}]
}

set maxValue [expr {$sideLength * $sideLength}]
set stepsFromEdge [expr {($sideLength - 1) / 2}]
set distanceToMiddle 0

for {set i 0} {$i < 4} {incr i} {
    set middlePoint [expr {$maxValue - $stepsFromEdge - ($sideLength - 1) * $i}]
    set distance [expr {abs($target - $middlePoint)}]
    if {$i == 0 || $distance < $distanceToMiddle} {
        set distanceToMiddle $distance
    }
}

set manhattanDistance [expr {$stepsFromEdge + $distanceToMiddle}]
puts $manhattanDistance