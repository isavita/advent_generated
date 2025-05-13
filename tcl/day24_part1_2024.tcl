
proc main {} {
    array set values {}
    set instructions {}

    set f [open "input.txt" r]
    set initial_section true
    while {[gets $f line] != -1} {
        set line [string trim $line]
        if {$line eq ""} continue

        set colon_pos [string first ":" $line]

        if {$initial_section} {
            if {$colon_pos != -1} {
                set parts [split $line ":"]
                set wire_name [string trim [lindex $parts 0]]
                set value [string trim [lindex $parts 1]]
                set values($wire_name) $value
            } else {
                set initial_section false
                lappend instructions $line
            }
        } else {
            if {$colon_pos == -1} {
                lappend instructions $line
            } else {
                lappend instructions $line
            }
        }
    }
    close $f

    while {true} {
        set changed false
        set remaining_instructions {}

        foreach instruction $instructions {
            set arrow_pos [string first "->" $instruction]
            if {$arrow_pos == -1} {
                lappend remaining_instructions $instruction
                continue
            }

            set parts [split $instruction "->"]
            set potential_output_wire [string trim [lindex $parts 1]]

            if {[info exists values($potential_output_wire)]} {
                 continue
            }

            set processed_this_round false

            if {[regexp {^([a-z0-9]+)\s+(AND|OR|XOR)\s+([a-z0-9]+)\s+->\s+([a-z0-9]+)$} $instruction -> in1_str op in2_str out_wire]} {
                set val1 [resolve_value $in1_str values]
                set val2 [resolve_value $in2_str values]

                if {$val1 ne "" && $val2 ne ""} {
                    set result 0
                    switch $op {
                        "AND" { set result [expr {$val1 & $val2}] }
                        "OR"  { set result [expr {$val1 | $val2}] }
                        "XOR" { set result [expr {$val1 ^ $val2}] }
                    }
                    set values($out_wire) $result
                    set changed true
                    set processed_this_round true
                }
            }

            if {!$processed_this_round && [regexp {^([a-z0-9]+)\s+->\s+([a-z0-9]+)$} $instruction -> in_str out_wire]} {
                set val [resolve_value $in_str values]

                if {$val ne ""} {
                    set values($out_wire) $val
                    set changed true
                    set processed_this_round true
                }
            }

            if {!$processed_this_round} {
                lappend remaining_instructions $instruction
            }
        }

        set instructions $remaining_instructions

        if {!$changed} break
        if {[llength $instructions] == 0} break
    }

    set binary_string ""
    for {set i 0} {true} {incr i} {
        set wire_name [format "z%02d" $i]
        if {![info exists values($wire_name)]} {
            break
        }
        set binary_string "$values($wire_name)$binary_string"
    }

    set decimal_value 0
    if {$binary_string ne ""} {
        catch {scan $binary_string %b decimal_value}
    }

    puts $decimal_value
}

proc resolve_value {input values_array_name} {
    upvar 1 $values_array_name values
    if {[string is integer -strict $input]} {
        return $input
    }
    if {[info exists values($input)]} {
        return $values($input)
    }
    return ""
}

if {[info exists ::argv0]} {
    main
}
