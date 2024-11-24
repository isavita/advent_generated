
#!/usr/bin/env tclsh

proc parseInput {input} {
    set histories {}
    foreach line $input {
        lappend histories [parseStringToInts $line]
    }
    return $histories
}

proc parseStringToInts {numbersLine} {
    return [lmap num [split $numbersLine] {expr {int($num)}}]
}

proc allZeros {nums} {
    foreach num $nums {
        if {$num != 0} {
            return 0
        }
    }
    return 1
}

proc calculateExtrapolation {history} {
    set extrapolations {}
    for {set i 1} {$i < [llength $history]} {incr i} {
        lappend extrapolations [expr {[lindex $history $i] - [lindex $history [expr {$i-1}]]}]
    }
    return $extrapolations
}

proc calculateExtrapolations {history} {
    set extrapolationsSeries [list $history]

    for {set i 1} {$i < [llength $history]} {incr i} {
        set previousExtrapolations [lindex $extrapolationsSeries [expr {$i-1}]]
        
        if {[allZeros $previousExtrapolations]} {
            return $extrapolationsSeries
        }

        lappend extrapolationsSeries [calculateExtrapolation $previousExtrapolations]
    }

    return $extrapolationsSeries
}

proc solve {input} {
    set histories [parseInput $input]
    set res 0

    foreach history $histories {
        set extrapolationsSeries [calculateExtrapolations $history]

        set futurePrediction 0
        for {set i [expr {[llength $extrapolationsSeries] - 1}]} {$i >= 0} {incr i -1} {
            set currentSeries [lindex $extrapolationsSeries $i]
            set futurePrediction [expr {[lindex $currentSeries end] + $futurePrediction}]
        }

        incr res $futurePrediction
    }

    return $res
}

# Read input from file
set fp [open "input.txt" r]
set input [split [read $fp] "\n"]
close $fp

# Remove any empty lines
set input [lsearch -all -inline -not -exact $input ""]

puts [solve $input]
