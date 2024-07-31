proc main {} {
    set file [open "input.txt" r]
    set nanobots {}
    while {[gets $file line] >= 0} {
        regexp {pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)} $line match x y z radius
        lappend nanobots [list $x $y $z $radius]
    }
    close $file

    set strongest [findStrongestNanobot $nanobots]
    set inRangeCount [countNanobotsInRange $nanobots $strongest]
    puts $inRangeCount
}

proc findStrongestNanobot {nanobots} {
    set strongest {}
    foreach nanobot $nanobots {
        if {[llength $strongest] == 0 || [lindex $nanobot 3] > [lindex $strongest 3]} {
            set strongest $nanobot
        }
    }
    return $strongest
}

proc countNanobotsInRange {nanobots strongest} {
    set count 0
    foreach nanobot $nanobots {
        if {[manhattanDistance $nanobot $strongest] <= [lindex $strongest 3]} {
            incr count
        }
    }
    return $count
}

proc manhattanDistance {a b} {
    return [expr {abs([lindex $a 0] - [lindex $b 0]) + abs([lindex $a 1] - [lindex $b 1]) + abs([lindex $a 2] - [lindex $b 2])}]
}

proc abs {x} {
    return [expr {$x < 0 ? -$x : $x}]
}

main