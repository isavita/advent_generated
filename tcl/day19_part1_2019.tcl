
proc load_program {filename code_array_name} {
    upvar 1 $code_array_name code
    array unset code
    set f [open $filename r]
    set data [read $f]
    close $f
    set values [split [string trim $data] ","]
    set i 0
    foreach val $values {
        set code($i) [string trim $val]
        incr i
    }
}

proc get_param {idx mode code_arr_name state_arr_name} {
    upvar 1 $code_arr_name code
    upvar 1 $state_arr_name state
    set ip $state(ip)
    set relative_base $state(relative_base)
    set val 0
    if {[info exists code($ip + $idx)]} {
        set val $code($ip + $idx)
    }
    if {$mode == 0} {
        if {[info exists code($val)]} {
            return $code($val)
        } else {
            return 0
        }
    } elseif {$mode == 1} {
        return $val
    } elseif {$mode == 2} {
        set addr [expr {$relative_base + $val}]
        if {[info exists code($addr)]} {
            return $code($addr)
        } else {
            return 0
        }
    }
    error "Unknown mode: $mode"
}

proc get_address {idx mode code_arr_name state_arr_name} {
    upvar 1 $code_arr_name code
    upvar 1 $state_arr_name state
    set ip $state(ip)
    set relative_base $state(relative_base)
    set val 0
    if {[info exists code($ip + $idx)]} {
        set val $code($ip + $idx)
    }
    if {$mode == 0} {
        return $val
    } elseif {$mode == 2} {
        return [expr {$relative_base + $val}]
    }
    error "Invalid mode for address: $mode"
}

proc run_vm {code_name state_name} {
    upvar 1 $code_name code
    upvar 1 $state_name state
    set ip $state(ip)
    set relative_base $state(relative_base)
    set input $state(input)
    set output $state(output)
    while {true} {
        set cmd 0
        if {[info exists code($ip)]} {
            set cmd $code($ip)
        }
        set opcode [expr {$cmd % 100}]
        set mode1 [expr {($cmd / 100) % 10}]
        set mode2 [expr {($cmd / 1000) % 10}]
        set mode3 [expr {($cmd / 10000) % 10}]
        switch $opcode {
            1 {
                set val1 [get_param 1 $mode1 $code_name $state_name]
                set val2 [get_param 2 $mode2 $code_name $state_name]
                set addr3 [get_address 3 $mode3 $code_name $state_name]
                set code($addr3) [expr {$val1 + $val2}]
                incr ip 4
            }
            2 {
                set val1 [get_param 1 $mode1 $code_name $state_name]
                set val2 [get_param 2 $mode2 $code_name $state_name]
                set addr3 [get_address 3 $mode3 $code_name $state_name]
                set code($addr3) [expr {$val1 * $val2}]
                incr ip 4
            }
            3 {
                set addr1 [get_address 1 $mode1 $code_name $state_name]
                if {[llength $input] == 0} {
                    error "Input buffer empty"
                }
                set input_val [lindex $input 0]
                set input [lreplace $input 0 0]
                set code($addr1) $input_val
                incr ip 2
            }
            4 {
                set val1 [get_param 1 $mode1 $code_name $state_name]
                lappend output $val1
                incr ip 2
            }
            5 {
                set val1 [get_param 1 $mode1 $code_name $state_name]
                set val2 [get_param 2 $mode2 $code_name $state_name]
                if {$val1 != 0} {
                    set ip $val2
                } else {
                    incr ip 3
                }
            }
            6 {
                set val1 [get_param 1 $mode1 $code_name $state_name]
                set val2 [get_param 2 $mode2 $code_name $state_name]
                if {$val1 == 0} {
                    set ip $val2
                } else {
                    incr ip 3
                }
            }
            7 {
                set val1 [get_param 1 $mode1 $code_name $state_name]
                set val2 [get_param 2 $mode2 $code_name $state_name]
                set addr3 [get_address 3 $mode3 $code_name $state_name]
                set code($addr3) [expr {$val1 < $val2 ? 1 : 0}]
                incr ip 4
            }
            8 {
                set val1 [get_param 1 $mode1 $code_name $state_name]
                set val2 [get_param 2 $mode2 $code_name $state_name]
                set addr3 [get_address 3 $mode3 $code_name $state_name]
                set code($addr3) [expr {$val1 == $val2 ? 1 : 0}]
                incr ip 4
            }
            9 {
                set val1 [get_param 1 $mode1 $code_name $state_name]
                incr relative_base $val1
                incr ip 2
            }
            99 {
                break
            }
            default {
                error "Unknown opcode: $opcode at address $ip"
            }
        }
    }
    set state(ip) $ip
    set state(relative_base) $relative_base
    set state(input) $input
    set state(output) $output
}

proc beam {initial_code_name x y} {
    upvar 1 $initial_code_name initial_code
    array set vm_code [array get initial_code]
    array set vm_state {ip 0 relative_base 0 input {} output {}}
    set vm_state(input) [list $x $y]
    run_vm vm_code vm_state
    set output $vm_state(output)
    if {[llength $output] == 1 && [lindex $output 0] == 1} {
        return 1
    } else {
        return 0
    }
}

proc main {} {
    array set initial_program_code {}
    load_program "input.txt" initial_program_code
    set count 0
    for {set y 0} {$y < 50} {incr y} {
        for {set x 0} {$x < 50} {incr x} {
            if {[beam initial_program_code $x $y]} {
                incr count
            }
        }
    }
    puts $count
}

main
