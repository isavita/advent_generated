set file [open "input.txt" r]
set reindeers {}

while {[gets $file line] >= 0} {
    regexp {(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.} $line -> name speed flyTime restTime
    lappend reindeers [list $name $speed $flyTime $restTime]
}
close $file

set maxDistance 0
set totalTime 2503

foreach reindeer $reindeers {
    set speed [lindex $reindeer 1]
    set flyTime [lindex $reindeer 2]
    set restTime [lindex $reindeer 3]

    set cycleTime [expr {$flyTime + $restTime}]
    set fullCycles [expr {$totalTime / $cycleTime}]
    set remainingTime [expr {$totalTime % $cycleTime}]

    set distance [expr {$fullCycles * $flyTime * $speed}]
    if {$remainingTime > $flyTime} {
        set distance [expr {$distance + $flyTime * $speed}]
    } else {
        set distance [expr {$distance + $remainingTime * $speed}]
    }

    if {$distance > $maxDistance} {
        set maxDistance $distance
    }
}

puts $maxDistance