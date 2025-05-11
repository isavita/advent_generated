
set code [dict create]
set ip 0
set input_list {}
set output_list {}
set relative_base 0

proc get_mem {addr} {
    global code
    if {[dict exists $code $addr]} {
        return [dict get $code $addr]
    } else {
        return 0
    }
}

proc set_mem {addr value} {
    global code
    dict set code $addr $value
}

proc get_param {index modes_list} {
    global ip relative_base
    set mode [lindex $modes_list [expr {$index - 1}]]
    set raw_val [get_mem [expr {$ip + $index}]]
    if {$mode == 0} {
        return [get_mem $raw_val]
    } elseif {$mode == 1} {
        return $raw_val
    } elseif {$mode == 2} {
        return [get_mem [expr {$relative_base + $raw_val}]]
    } else {
        error "Unknown mode: $mode"
    }
}

proc get_address {index modes_list} {
    global ip relative_base
    set mode [lindex $modes_list [expr {$index - 1}]]
    set raw_val [get_mem [expr {$ip + $index}]]
    if {$mode == 0} {
        return $raw_val
    } elseif {$mode == 2} {
        return [expr {$relative_base + $raw_val}]
    } else {
        error "Invalid mode for address: $mode"
    }
}

proc load_code {filename} {
    global code
    set f [open $filename r]
    set data [read $f]
    close $f
    set vals [split [string trim $data] ","]
    set i 0
    foreach val $vals {
        dict set code $i [string trim $val]
        incr i
    }
}

proc send_string {s} {
    global input_list
    foreach c [split $s ""] {
        lappend input_list [scan $c %c]
    }
    lappend input_list 10
}

proc run_vm {} {
    global code ip input_list output_list relative_base
    while {true} {
        set cmd [get_mem $ip]
        set opcode [expr {$cmd % 100}]
        set modes [list \
            [expr {($cmd / 100) % 10}] \
            [expr {($cmd / 1000) % 10}] \
            [expr {($cmd / 10000) % 10}] \
        ]

        switch -- $opcode {
            1 {
                set param1 [get_param 1 $modes]
                set param2 [get_param 2 $modes]
                set dest_addr [get_address 3 $modes]
                set_mem $dest_addr [expr {$param1 + $param2}]
                incr ip 4
            }
            2 {
                set param1 [get_param 1 $modes]
                set param2 [get_param 2 $modes]
                set dest_addr [get_address 3 $modes]
                set_mem $dest_addr [expr {$param1 * $param2}]
                incr ip 4
            }
            3 {
                if {[llength $input_list] == 0} {
                    error "Input list empty at ip $ip"
                }
                set input_val [lindex $input_list 0]
                set input_list [lrange $input_list 1 end]
                set dest_addr [get_address 1 $modes]
                set_mem $dest_addr $input_val
                incr ip 2
            }
            4 {
                set param1 [get_param 1 $modes]
                lappend output_list $param1
                incr ip 2
            }
            5 {
                set param1 [get_param 1 $modes]
                set param2 [get_param 2 $modes]
                if {$param1 != 0} {
                    set ip $param2
                } else {
                    incr ip 3
                }
            }
            6 {
                set param1 [get_param 1 $modes]
                set param2 [get_param 2 $modes]
                if {$param1 == 0} {
                    set ip $param2
                } else {
                    incr ip 3
                }
            }
            7 {
                set param1 [get_param 1 $modes]
                set param2 [get_param 2 $modes]
                set dest_addr [get_address 3 $modes]
                set_mem $dest_addr [expr {$param1 < $param2 ? 1 : 0}]
                incr ip 4
            }
            8 {
                set param1 [get_param 1 $modes]
                set param2 [get_param 2 $modes]
                set dest_addr [get_address 3 $modes]
                set_mem $dest_addr [expr {$param1 == $param2 ? 1 : 0}]
                incr ip 4
            }
            9 {
                set param1 [get_param 1 $modes]
                incr relative_base $param1
                incr ip 2
            }
            99 {
                break
            }
            default {
                error "Unknown opcode $opcode at ip $ip"
            }
        }
    }
}

proc main {} {
    global code ip input_list output_list relative_base
    set ip 0
    set input_list {}
    set output_list {}
    set relative_base 0

    load_code "input.txt"

    set instructions [list \
        "NOT A J" \
        "NOT B T" \
        "OR T J" \
        "NOT C T" \
        "OR T J" \
        "AND D J" \
        "NOT A T" \
        "AND A T" \
        "OR E T" \
        "OR H T" \
        "AND T J" \
        "RUN" \
    ]

    foreach instr $instructions {
        send_string $instr
    }

    run_vm

    foreach output_val $output_list {
        puts $output_val
    }
}

main
