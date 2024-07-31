proc react {polymer} {
    set stack {}
    foreach unit [split $polymer ""] {
        if {[llength $stack] > 0} {
            set top [lindex $stack end]
            if {[string equal [string toupper $unit] [string toupper $top]] && [string compare $unit $top] != 0} {
                set stack [lrange $stack 0 end-1]
            } else {
                lappend stack $unit
            }
        } else {
            lappend stack $unit
        }
    }
    return [llength $stack]
}

set input [read [open "input.txt"]]
set result [react $input]
puts $result