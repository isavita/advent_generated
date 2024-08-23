proc readInput {filename} {
    set file [open $filename r]
    set line [gets $file]
    close $file
    return [split $line]
}

proc redistribute {banks} {
    set seen [dict create]
    set cycles 0

    while {1} {
        set config [join $banks ","]
        if {[dict exists $seen $config]} {
            return [expr {$cycles - [dict get $seen $config]}]
        }
        dict set seen $config $cycles

        set maxBlocks 0
        set maxIndex 0
        for {set i 0} {$i < [llength $banks]} {incr i} {
            if {[lindex $banks $i] > $maxBlocks} {
                set maxBlocks [lindex $banks $i]
                set maxIndex $i
            }
        }

        set blocks [lindex $banks $maxIndex]
        set banks [lreplace $banks $maxIndex $maxIndex 0]
        set index [expr {$maxIndex + 1}]
        while {$blocks > 0} {
            if {$index >= [llength $banks]} {
                set index 0
            }
            set banks [lreplace $banks $index $index [expr {[lindex $banks $index] + 1}]]
            incr index
            incr blocks -1
        }
        incr cycles
    }
}

set banks [readInput "input.txt"]
set loopSize [redistribute $banks]
puts $loopSize