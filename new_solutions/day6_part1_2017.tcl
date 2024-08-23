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
            return $cycles
        }
        dict set seen $config 1
        set cycles [expr {$cycles + 1}]

        set maxIndex 0
        set maxBlocks 0
        for {set i 0} {$i < [llength $banks]} {incr i} {
            if {[lindex $banks $i] > $maxBlocks} {
                set maxBlocks [lindex $banks $i]
                set maxIndex $i
            }
        }

        set banks [lreplace $banks $maxIndex $maxIndex 0]
        for {set i 0} {$i < $maxBlocks} {incr i} {
            set idx [expr {($maxIndex + $i + 1) % [llength $banks]}]
            set banks [lreplace $banks $idx $idx [expr {[lindex $banks $idx] + 1}]]
        }
    }
}

set banks [readInput "input.txt"]
puts [redistribute $banks]