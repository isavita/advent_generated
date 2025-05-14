
# Declare global state variables for the machine
variable memory
variable ip
variable relbase
variable input_queue
variable output_list

# Initialize machine state
proc init_machine {initial_program_parts} {
    variable memory
    variable ip
    variable relbase
    variable input_queue
    variable output_list

    unset -nocomplain memory
    unset -nocomplain input_queue
    unset -nocomplain output_list

    array set memory {}
    set i 0
    foreach part $initial_program_parts {
        set memory($i) [string trim $part]
        incr i
    }

    set ip 0
    set relbase 0
    set input_queue [list]
    set output_list [list]
}

# Helper to get memory value, handling modes and default 0
proc get_value {addr mode} {
    variable memory
    variable relbase

    set val 0
    set deref_addr 0

    if {$mode == 0} {
        if {[info exists memory($addr)]} {
            set deref_addr $memory($addr)
        }
        if {[info exists memory($deref_addr)]} {
            set val $memory($deref_addr)
        }
    } elseif {$mode == 1} {
        if {[info exists memory($addr)]} {
            set val $memory($addr)
        }
    } elseif {$mode == 2} {
         set offset 0
         if {[info exists memory($addr)]} {
            set offset $memory($addr)
        }
        set deref_addr [expr {$relbase + $offset}]
         if {[info exists memory($deref_addr)]} {
            set val $memory($deref_addr)
        }
    }
    return $val
}

# Helper to get write address, handling modes
proc get_write_addr {addr mode} {
    variable memory
    variable relbase

    set address_offset 0
    if {[info exists memory($addr)]} {
        set address_offset $memory($addr)
    }

    set write_addr 0
    if {$mode == 0} {
        set write_addr $address_offset
    } elseif {$mode == 2} {
        set write_addr [expr {$relbase + $address_offset}]
    } else {
        error "Invalid write mode: $mode at address $addr"
    }
    return $write_addr
}

# Helper to set memory value
proc set_value {addr val} {
    variable memory
    set memory($addr) $val
}

# Decode instruction
proc decode_instruction {instr} {
    set opcode [expr {$instr % 100}]
    set modes [list]
    set instr [expr {$instr / 100}]
    lappend modes [expr {$instr % 10}]
    set instr [expr {$instr / 10}]
    lappend modes [expr {$instr % 10}]
    set instr [expr {$instr / 10}]
    lappend modes [expr {$instr % 10}]
    return [list $opcode $modes]
}

# Execute one step
proc step {} {
    variable memory
    variable ip
    variable relbase
    variable input_queue
    variable output_list

    set instruction 0
    if {[info exists memory($ip)]} {
         set instruction $memory($ip)
    }

    if {$instruction == 99} {
        return false
    }

    set decoded [decode_instruction $instruction]
    set opcode [lindex $decoded 0]
    set modes [lindex $decoded 1]

    while {[llength $modes] < 3} { lappend modes 0 }

    set mode1 [lindex $modes 0]
    set mode2 [lindex $modes 1]
    set mode3 [lindex $modes 2]

    switch $opcode {
        1 {
            set val1 [get_value [expr {$ip + 1}] $mode1]
            set val2 [get_value [expr {$ip + 2}] $mode2]
            set write_addr [get_write_addr [expr {$ip + 3}] $mode3]
            set_value $write_addr [expr {$val1 + $val2}]
            incr ip 4
        }
        2 {
            set val1 [get_value [expr {$ip + 1}] $mode1]
            set val2 [get_value [expr {$ip + 2}] $mode2]
            set write_addr [get_write_addr [expr {$ip + 3}] $mode3]
            set_value $write_addr [expr {$val1 * $val2}]
            incr ip 4
        }
        3 {
            if {[llength $input_queue] == 0} {
                 error "Input required but input queue is empty at address $ip"
            }
            set input_val [lindex $input_queue 0]
            set input_queue [lreplace $input_queue 0 0]
            set write_addr [get_write_addr [expr {$ip + 1}] $mode1]
            set_value $write_addr $input_val
            incr ip 2
        }
        4 {
            set output_val [get_value [expr {$ip + 1}] $mode1]
            lappend output_list $output_val
            incr ip 2
        }
        5 {
            set val1 [get_value [expr {$ip + 1}] $mode1]
            set val2 [get_value [expr {$ip + 2}] $mode2]
            if {$val1 != 0} {
                set ip $val2
            } else {
                incr ip 3
            }
        }
        6 {
            set val1 [get_value [expr {$ip + 1}] $mode1]
            set val2 [get_value [expr {$ip + 2}] $mode2]
            if {$val1 == 0} {
                set ip $val2
            } else {
                incr ip 3
            }
        }
        7 {
            set val1 [get_value [expr {$ip + 1}] $mode1]
            set val2 [get_value [expr {$ip + 2}] $mode2]
            set write_addr [get_write_addr [expr {$ip + 3}] $mode3]
            set_value $write_addr [expr {$val1 < $val2 ? 1 : 0}]
            incr ip 4
        }
        8 {
            set val1 [get_value [expr {$ip + 1}] $mode1]
            set val2 [get_value [expr {$ip + 2}] $mode2]
            set write_addr [get_write_addr [expr {$ip + 3}] $mode3]
            set_value $write_addr [expr {$val1 == $val2 ? 1 : 0}]
            incr ip 4
        }
        9 {
            set val1 [get_value [expr {$ip + 1}] $mode1]
            incr relbase $val1
            incr ip 2
        }
        default {
            error "Unknown opcode: $opcode at address $ip"
        }
    }
    return true
}

# Run the machine
proc run_machine {} {
    while {[step]} {}
    variable output_list
    return $output_list
}

# Parse output into grid
proc parse_scaffolding {output_list} {
    set grid [dict create]
    set x 0
    set y 0
    foreach o $output_list {
        set char [format %c $o]
        if {$char eq "\n"} {
            incr y
            set x 0
        } else {
            if {$char eq "#" || $char eq "^" || $char eq "v" || $char eq "<" || $char eq ">"} {
                dict set grid "$x,$y" "#"
            }
            incr x
        }
    }
    return $grid
}

# Calculate alignment parameter sum
proc sum_alignment_parameters {grid} {
    set sum 0
    dict for {coords value} $grid {
        set parts [split $coords ","]
        set x [lindex $parts 0]
        set y [lindex $parts 1]

        if {[dict exists $grid "$x,[expr {int($y) + 1}]"] && \
            [dict exists $grid "$x,[expr {int($y) - 1}]"] && \
            [dict exists $grid "[expr {int($x) + 1}],$y"] && \
            [dict exists $grid "[expr {int($x) - 1}],$y"]} {

            incr sum [expr {int($x) * int($y)}]
        }
    }
    return $sum
}

# Main procedure
proc main {} {
    set filename "input.txt"
    set f [open $filename r]
    set line [read $f]
    close $f

    set program_parts [split [string trim $line] ","]

    init_machine $program_parts
    set output [run_machine]

    set scaffolding_grid [parse_scaffolding $output]

    set alignment_sum [sum_alignment_parameters $scaffolding_grid]
    puts $alignment_sum
}

# Execute main procedure
main
