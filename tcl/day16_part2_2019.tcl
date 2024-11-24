
#!/usr/bin/env tclsh

proc repeatInput {input times} {
    set digits {}
    for {set t 0} {$t < $times} {incr t} {
        foreach char [split $input ""] {
            lappend digits [scan $char %d]
        }
    }
    return $digits
}

proc main {} {
    # Read input file
    set file [open "input.txt" r]
    set input [string trim [read $file]]
    close $file

    # Repeat input signal
    set repeatedInput [repeatInput $input 10000]

    # Find message offset
    set offset [string range $input 0 6]
    set offset [scan $offset %d]

    # Apply FFT for 100 phases
    for {set phase 0} {$phase < 100} {incr phase} {
        set sum 0
        for {set i [expr {[llength $repeatedInput] - 1}]} {$i >= $offset} {incr i -1} {
            incr sum [lindex $repeatedInput $i]
            lset repeatedInput $i [expr {$sum % 10}]
        }
    }

    # Print eight-digit message
    set message ""
    for {set i $offset} {$i < $offset + 8} {incr i} {
        append message [lindex $repeatedInput $i]
    }
    puts $message
}

main
