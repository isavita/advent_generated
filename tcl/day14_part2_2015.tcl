proc readReindeerDetails {filename} {
    set reindeers {}
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        set parts [split $line]
        lappend reindeers [list [lindex $parts 3] [lindex $parts 6] [lindex $parts 13] 0 0 1 0]
    }
    close $file
    return $reindeers
}

proc simulateRaceWithPoints {reindeers totalSeconds} {
    for {set i 0} {$i < $totalSeconds} {incr i} {
        set maxDistance 0
        for {set j 0} {$j < [llength $reindeers]} {incr j} {
            set reindeer [lindex $reindeers $j]
            set speed [lindex $reindeer 0]
            set flyTime [lindex $reindeer 1]
            set restTime [lindex $reindeer 2]
            set distance [lindex $reindeer 3]
            set points [lindex $reindeer 4]
            set flying [lindex $reindeer 5]
            set timeInMode [lindex $reindeer 6]

            if {$flying} {
                set distance [expr {$distance + $speed}]
            }
            set timeInMode [expr {$timeInMode + 1}]
            if {$flying && $timeInMode == $flyTime} {
                set flying 0
                set timeInMode 0
            } elseif {!$flying && $timeInMode == $restTime} {
                set flying 1
                set timeInMode 0
            }
            set reindeer [list $speed $flyTime $restTime $distance $points $flying $timeInMode]
            set reindeers [lreplace $reindeers $j $j $reindeer]
            if {$distance > $maxDistance} {
                set maxDistance $distance
            }
        }
        for {set j 0} {$j < [llength $reindeers]} {incr j} {
            if {[lindex [lindex $reindeers $j] 3] == $maxDistance} {
                set points [lindex [lindex $reindeers $j] 4]
                set points [expr {$points + 1}]
                set reindeer [lreplace [lindex $reindeers $j] 4 4 $points]
                set reindeers [lreplace $reindeers $j $j $reindeer]
            }
        }
    }
    return $reindeers
}

proc findMaxPoints {reindeers} {
    set maxPoints 0
    foreach reindeer $reindeers {
        set points [lindex $reindeer 4]
        if {$points > $maxPoints} {
            set maxPoints $points
        }
    }
    return $maxPoints
}

set reindeers [readReindeerDetails "input.txt"]
set reindeers [simulateRaceWithPoints $reindeers 2503]
puts [findMaxPoints $reindeers]