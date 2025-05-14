
proc get_value {memory_ref address} {
    upvar 1 $memory_ref memory
    if {[info exists memory($address)]} {return $memory($address)} else {return 0}
}

proc set_value {memory_ref address value} {
    upvar 1 $memory_ref memory
    set memory($address) $value
}

proc get_param {memory_ref ip_ref rb_ref offset mode} {
    upvar 1 $memory_ref memory
    upvar 1 $ip_ref ip
    upvar 1 $rb_ref rb
    set address [get_value memory [expr {$ip+$offset}]]
    switch -- $mode {
        0 { return [get_value memory $address] }
        1 { return $address }
        2 { return [get_value memory [expr {$rb+$address}]] }
        default { error "Invalid parameter mode $mode" }
    }
}

proc get_write_address {memory_ref ip_ref rb_ref offset mode} {
    upvar 1 $memory_ref memory
    upvar 1 $ip_ref ip
    upvar 1 $rb_ref rb
    set address [get_value memory [expr {$ip+$offset}]]
    switch -- $mode {
        0 { return $address }
        2 { return [expr {$rb+$address}] }
        default { error "Invalid write address mode $mode" }
    }
}

proc intcode_next {memory_ref ip_ref rb_ref input_cmd} {
    upvar 1 $memory_ref memory
    upvar 1 $ip_ref ip
    upvar 1 $rb_ref rb

    while {1} {
        set instruction [get_value memory $ip]
        set opcode [expr {$instruction % 100}]
        set mode_p1 [expr {($instruction / 100) % 10}]
        set mode_p2 [expr {($instruction / 1000) % 10}]
        set mode_p3 [expr {($instruction / 10000) % 10}]

        if {$opcode == 99} {
            return "HALT"
        }

        if {$opcode == 3} {
            set address [get_write_address memory ip rb 1 $mode_p1]
            set input_value [uplevel 1 $input_cmd]
            set_value memory $address $input_value
            incr ip 2
        } elseif {$opcode == 4} {
            set output_value [get_param memory ip rb 1 $mode_p1]
            incr ip 2
            return $output_value
        } else {
            set p1 [get_param memory ip rb 1 $mode_p1]
            if {$opcode != 9} {
                 set p2 [get_param memory ip rb 2 $mode_p2]
            }

            if {$opcode == 1 || $opcode == 2 || $opcode == 7 || $opcode == 8} {
                 set write_addr [get_write_address memory ip rb 3 $mode_p3]
            }

            switch -- $opcode {
                1 {
                    set_value memory $write_addr [expr {$p1 + $p2}]
                    incr ip 4
                }
                2 {
                    set_value memory $write_addr [expr {$p1 * $p2}]
                    incr ip 4
                }
                5 {
                    if {$p1 != 0} { set ip $p2 } else { incr ip 3 }
                }
                6 {
                    if {$p1 == 0} { set ip $p2 } else { incr ip 3 }
                }
                7 {
                    set_value memory $write_addr [expr {$p1 < $p2 ? 1 : 0}]
                    incr ip 4
                }
                8 {
                    set_value memory $write_addr [expr {$p1 == $p2 ? 1 : 0}]
                    incr ip 4
                }
                9 {
                    incr rb $p1
                    incr ip 2
                }
                default { error "Unknown opcode $opcode at ip $ip" }
            }
        }
    }
}

proc main {} {
    set f [open input.txt r]
    set data [read $f]
    close $f

    set program_list [split [string trim $data] ","]
    array set memory {}
    for {set i 0} {$i < [llength $program_list]} {incr i} {
        set memory($i) [lindex $program_list $i]
    }

    set memory(0) 2

    set ip 0
    set rb 0
    variable ballX 0
    variable paddleX 0
    variable score 0
    variable outputs_buffer {}

    set input_cmd {expr {$::ballX > $::paddleX ? 1 : ($::ballX < $::paddleX ? -1 : 0)}}

    while {1} {
        set output [intcode_next memory ip rb $input_cmd]
        if {$output eq "HALT"} { break }

        lappend outputs_buffer $output

        if {[llength $outputs_buffer] == 3} {
            set x [lindex $outputs_buffer 0]
            set y [lindex $outputs_buffer 1]
            set tileId [lindex $outputs_buffer 2]
            set outputs_buffer {}

            if {$x == -1 && $y == 0} {
                set score $tileId
            } else {
                if {$tileId == 3} { set paddleX $x }
                if {$tileId == 4} { set ballX $x }
            }
        }
    }

    puts $score
}

main
