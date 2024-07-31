set input [read [open "input.txt"]]
set digits [split $input ""]
set digits [lmap d $digits {expr {$d + 0}}]

proc applyFFT {input} {
    set basePattern {0 1 0 -1}
    set output {}
    set n [llength $input]
    
    for {set i 0} {$i < $n} {incr i} {
        set sum 0
        for {set j 0} {$j < $n} {incr j} {
            set patternValue [lindex $basePattern [expr {($j + 1) / ($i + 1) % 4}]]
            set sum [expr {$sum + [lindex $input $j] * $patternValue}]
        }
        lappend output [expr {abs($sum) % 10}]
    }
    return $output
}

for {set phase 0} {$phase < 100} {incr phase} {
    set digits [applyFFT $digits]
}

puts [join [lrange $digits 0 7] ""]