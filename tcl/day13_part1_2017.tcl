proc calculate_severity {filename} {
    array set layers {}
    set max_depth 0

    # Read input file
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        set parts [split $line ":"]
        set depth [lindex $parts 0]
        set range [lindex $parts 1]
        set layers($depth) $range
        if {$depth > $max_depth} {
            set max_depth $depth
        }
    }
    close $file

    set severity 0

    # Calculate severity
    for {set depth 0} {$depth <= $max_depth} {incr depth} {
        if {[info exists layers($depth)]} {
            set range $layers($depth)
            set period [expr {$range * 2 - 2}]
            if {$depth % $period == 0} {
                set severity [expr {$severity + $depth * $range}]
            }
        }
    }

    return $severity
}

# Main execution
set severity [calculate_severity "input.txt"]
puts $severity