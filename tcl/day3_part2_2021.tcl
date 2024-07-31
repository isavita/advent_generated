set file [open "input.txt" r]
set values [split [read $file] "\n"]
close $file

proc filterValues {values criteria} {
    set length [string length [lindex $values 0]]
    for {set i 0} {$i < $length} {incr i} {
        set zeros 0
        set ones 0
        foreach val $values {
            if {[string index $val $i] == "0"} {
                incr zeros
            } else {
                incr ones
            }
        }
        set keep [expr {$criteria == "oxygen" ? ($zeros > $ones ? "1" : "0") : ($zeros <= $ones ? "1" : "0")}]
        set values [filterByBit $values $i $keep]
        if {[llength $values] == 1} {
            break
        }
    }
    return [lindex $values 0]
}

proc filterByBit {values bitIndex keep} {
    set filtered {}
    foreach val $values {
        if {[string index $val $bitIndex] == $keep} {
            lappend filtered $val
        }
    }
    return $filtered
}

set oxygenGeneratorRating [filterValues $values "oxygen"]
set co2ScrubberRating [filterValues $values "co2"]

set oxygenGeneratorRatingInt [scan $oxygenGeneratorRating %b]
set co2ScrubberRatingInt [scan $co2ScrubberRating %b]

puts [expr {$oxygenGeneratorRatingInt * $co2ScrubberRatingInt}]