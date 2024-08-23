proc readInput {filename} {
    set file [open $filename r]
    set line [gets $file]
    close $file
    return [split $line ","]
}

proc playGame {startingNumbers targetTurn} {
    array set lastSpoken {}
    set lastNumber 0

    for {set i 0} {$i < [llength $startingNumbers]} {incr i} {
        set num [lindex $startingNumbers $i]
        if {$i < [llength $startingNumbers] - 1} {
            set lastSpoken($num) [expr {$i + 1}]
        }
        set lastNumber $num
    }

    set turn [expr {[llength $startingNumbers] + 1}]
    for {} {$turn <= $targetTurn} {incr turn} {
        if {[info exists lastSpoken($lastNumber)]} {
            set nextNumber [expr {$turn - 1 - $lastSpoken($lastNumber)}]
        } else {
            set nextNumber 0
        }
        set lastSpoken($lastNumber) [expr {$turn - 1}]
        set lastNumber $nextNumber
    }

    return $lastNumber
}

set startingNumbers [readInput "input.txt"]
set result [playGame $startingNumbers 30000000]
puts $result