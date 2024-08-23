proc main {} {
    array set bots {}
    array set outputs {}

    set file [open "input.txt" r]
    set instructions [split [read $file] "\n"]
    close $file

    foreach line $instructions {
        if {[regexp {value (\d+) goes to bot (\d+)} $line -> value bot]} {
            if {![info exists bots($bot)]} {
                set bots($bot) {}
            }
            lappend bots($bot) $value
        } elseif {[regexp {bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)} $line -> bot lowType lowDest highType highDest]} {
            set instructionsList($bot) [list $lowType $lowDest $highType $highDest]
        }
    }

    while {1} {
        set found 0
        foreach bot [array names bots] {
            set chips [lsort -integer [set bots($bot)]]
            if {[llength $chips] == 2} {
                set found 1
                set low [lindex $chips 0]
                set high [lindex $chips 1]
                set inst [set instructionsList($bot)]

                if {[lindex $inst 0] eq "bot"} {
                    if {![info exists bots([lindex $inst 1])]} {
                        set bots([lindex $inst 1]) {}
                    }
                    lappend bots([lindex $inst 1]) $low
                } else {
                    if {![info exists outputs([lindex $inst 1])]} {
                        set outputs([lindex $inst 1]) {}
                    }
                    lappend outputs([lindex $inst 1]) $low
                }

                if {[lindex $inst 2] eq "bot"} {
                    if {![info exists bots([lindex $inst 3])]} {
                        set bots([lindex $inst 3]) {}
                    }
                    lappend bots([lindex $inst 3]) $high
                } else {
                    if {![info exists outputs([lindex $inst 3])]} {
                        set outputs([lindex $inst 3]) {}
                    }
                    lappend outputs([lindex $inst 3]) $high
                }

                set bots($bot) {}
            }
        }
        if {!$found} {break}
    }

    set product 1
    foreach i {0 1 2} {
        if {[info exists outputs($i)]} {
            set product [expr {$product * [lindex $outputs($i) 0]}]
        }
    }
    puts $product
}

main