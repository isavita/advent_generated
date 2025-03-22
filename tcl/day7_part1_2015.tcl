
# --- Day 7: Some Assembly Required ---

proc evaluate_circuit {instructions} {
    array set signals {}

    # Function to get the value of a wire or a literal
    proc get_value {val} {
        upvar signals signals
        if {[string is integer -strict $val]} {
            return $val
        } elseif {[info exists signals($val)]} {
            return $signals($val)
        } else {
            return "" ; # Return empty string if signal is not yet available
        }
    }
    
    # Iterate until we can fully evaluate wire "a"
    while {![info exists signals(a)] || [expr {$signals(a) eq ""}]} {
      set changed 0  ;# Flag to track whether changes happened in an iteration.
        foreach instruction $instructions {
            set parts [split $instruction " "]
            set len [llength $parts]
            
            if {$len == 3} {
                # value -> wire
                set value [get_value [lindex $parts 0]]
                set wire [lindex $parts 2]
                if {$value ne ""} {
                  set signals($wire) [expr {$value & 0xFFFF}]
                  set changed 1 ;# Mark a signal got updated
                }

            } elseif {$len == 4} {
                # NOT wire -> wire
                set op [lindex $parts 0]
                set arg [get_value [lindex $parts 1]]
                set wire [lindex $parts 3]

                if {$arg ne ""} {
                    set signals($wire) [expr {~$arg & 0xFFFF}]
                     set changed 1 ;# Mark a signal got updated
                }

            } elseif {$len == 5} {
                # wire OP wire -> wire
                set arg1 [get_value [lindex $parts 0]]
                set op [lindex $parts 1]
                set arg2 [get_value [lindex $parts 2]]
                set wire [lindex $parts 4]

                if {$arg1 ne "" && $arg2 ne ""} {
                    if {$op eq "AND"} {
                        set signals($wire) [expr {$arg1 & $arg2 & 0xFFFF}]
                    } elseif {$op eq "OR"} {
                        set signals($wire) [expr {$arg1 | $arg2 & 0xFFFF}]
                    } elseif {$op eq "LSHIFT"} {
                        set signals($wire) [expr {($arg1 << $arg2) & 0xFFFF}]
                    } elseif {$op eq "RSHIFT"} {
                        set signals($wire) [expr {$arg1 >> $arg2 & 0xFFFF}]
                    }
                     set changed 1 ;# Mark a signal got updated
                }
            }
        }
      
      # If no signal was updated in one pass, we are likely in deadlock
      if {$changed == 0 && (![info exists signals(a)] || [expr {$signals(a) eq ""}])} {
        puts stderr "Deadlock or unresolvable circuit detected."
        return ""
      }
    }
    return $signals(a)
}


proc main {} {
    set f [open "input.txt" r]
    set instructions [split [read $f] "\n"]
    close $f

    set result [evaluate_circuit $instructions]
    puts "Signal on wire a: $result"
}

main
