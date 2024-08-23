set file [open "input.txt" r]
set lines [split [read $file] "\n"]
close $file

set bitCount [string length [lindex $lines 0]]
set gammaRate 0
set epsilonRate 0

for {set i 0} {$i < $bitCount} {incr i} {
    set ones 0
    set zeros 0
    foreach line $lines {
        if {[string index $line $i] == "1"} {
            incr ones
        } else {
            incr zeros
        }
    }
    if {$ones > $zeros} {
        set gammaRate [expr {$gammaRate + (1 << ($bitCount - $i - 1))}]
    } else {
        set epsilonRate [expr {$epsilonRate + (1 << ($bitCount - $i - 1))}]
    }
}

set powerConsumption [expr {$gammaRate * $epsilonRate}]
puts $powerConsumption