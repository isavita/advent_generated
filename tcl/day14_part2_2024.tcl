
proc mod {a b} {
    expr {($a % $b + $b) % $b}
}

proc parseLine {line} {
    if {[regexp {p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)} $line -> x y vx vy]} {
        return [list $x $y $vx $vy]
    } else {
        return ""
    }
}

proc moveRobots {robots sizeX sizeY} {
    set newRobots {}
    foreach robot $robots {
        lassign $robot x y vx vy
        set newX [mod [expr {$x + $vx}] $sizeX]
        set newY [mod [expr {$y + $vy}] $sizeY]
        lappend newRobots [list $newX $newY $vx $vy]
    }
    return $newRobots
}

proc countQuadrants {robots sizeX sizeY} {
    set counts [list 0 0 0 0]
    set centerX [expr {$sizeX / 2}]
    set centerY [expr {$sizeY / 2}]
    foreach robot $robots {
        lassign $robot x y vx vy
        if {$x < $centerX} {
            if {$y < $centerY} {
                lset counts 0 [expr {[lindex $counts 0] + 1}]
            } elseif {$y > $centerY} {
                lset counts 1 [expr {[lindex $counts 1] + 1}]
            }
        } elseif {$x > $centerX} {
            if {$y < $centerY} {
                lset counts 2 [expr {[lindex $counts 2] + 1}]
            } elseif {$y > $centerY} {
                lset counts 3 [expr {[lindex $counts 3] + 1}]
            }
        }
    }
    return $counts
}

proc hasNoOverlaps {robots} {
    set positions {}
    foreach robot $robots {
        lassign $robot x y vx vy
        set pos "$x,$y"
        if {[dict exists $positions $pos]} {
            return 0
        }
        dict set positions $pos 1
    }
    return 1
}

proc drawGrid {robots sizeX sizeY} {
    set gridMap {}
    foreach robot $robots {
        lassign $robot x y vx vy
        dict set gridMap "$x,$y" 1
    }
    for {set y 0} {$y < $sizeY} {incr y} {
        set line ""
        for {set x 0} {$x < $sizeX} {incr x} {
            if {[dict exists $gridMap "$x,$y"]} {
                append line "#"
            } else {
                append line "."
            }
        }
        puts $line
    }
}

set sizeX 101
set sizeY 103

set file [open "input.txt" r]
set robots {}
while {[gets $file line] != -1} {
    if {$line eq ""} continue
    set robot [parseLine $line]
    if {$robot eq ""} {
        puts stderr "Error parsing line: $line"
        exit 1
    }
    lappend robots $robot
}
close $file

set robotsPart1 $robots
for {set n 0} {$n < 100} {incr n} {
    set robotsPart1 [moveRobots $robotsPart1 $sizeX $sizeY]
}

set counts [countQuadrants $robotsPart1 $sizeX $sizeY]
set safetyFactor 1
foreach c $counts {
    set safetyFactor [expr {$safetyFactor * $c}]
}
puts "Part 1 - Safety Factor after 100 seconds: $safetyFactor"

set robotsPart2 $robots
set seconds 0
while 1 {
    if {[hasNoOverlaps $robotsPart2]} {
        break
    }
    set robotsPart2 [moveRobots $robotsPart2 $sizeX $sizeY]
    incr seconds
    if {$seconds > 1000000} {
        puts "Exceeded maximum iterations without finding a unique position configuration."
        exit 1
    }
}
puts "Part 2 - Fewest seconds to display Easter egg: $seconds"
puts "Final positions of robots:"
drawGrid $robotsPart2 $sizeX $sizeY
