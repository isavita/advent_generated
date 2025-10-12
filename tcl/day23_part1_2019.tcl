set f [open "input.txt" r]
set data [read $f]
close $f
set program [split [string trim $data] ,]

proc get_param {memory ip relative_base mode offset} {
    set addr [expr {$ip + $offset}]
    set val [dict get $memory $addr]
    switch $mode {
        0 { return [dict get $memory $val] }
        1 { return $val }
        2 { return [dict get $memory [expr {$relative_base + $val}]] }
    }
}

proc set_param {memory ip relative_base mode offset value} {
    set addr [expr {$ip + $offset}]
    set val [dict get $memory $addr]
    switch $mode {
        0 { dict set memory $val $value }
        2 { dict set memory [expr {$relative_base + $val}] $value }
    }
    return $memory
}

proc run_intcode {memory ip relative_base inputs outputs} {
    upvar $inputs input_queue
    upvar $outputs output_list
    upvar $memory mem
    upvar $ip instruction_ptr
    upvar $relative_base base

    while {true} {
        set opcode [expr {[dict get $mem $instruction_ptr] % 100}]
        set modes [list \
            [expr {([dict get $mem $instruction_ptr] / 100) % 10}] \
            [expr {([dict get $mem $instruction_ptr] / 1000) % 10}] \
            [expr {([dict get $mem $instruction_ptr] / 10000) % 10}]]

        if {$opcode == 99} {
            return "halted"
        } elseif {$opcode in {1 2 7 8}} {
            set p1 [get_param $mem $instruction_ptr $base [lindex $modes 0] 1]
            set p2 [get_param $mem $instruction_ptr $base [lindex $modes 1] 2]
            if {$opcode == 1} {
                set result [expr {$p1 + $p2}]
            } elseif {$opcode == 2} {
                set result [expr {$p1 * $p2}]
            } elseif {$opcode == 7} {
                set result [expr {$p1 < $p2}]
            } elseif {$opcode == 8} {
                set result [expr {$p1 == $p2}]
            }
            set mem [set_param $mem $instruction_ptr $base [lindex $modes 2] 3 $result]
            incr instruction_ptr 4
        } elseif {$opcode == 3} {
            if {[llength $input_queue] == 0} {
                return "needs_input"
            }
            set value [lindex $input_queue 0]
            set input_queue [lrange $input_queue 1 end]
            set mem [set_param $mem $instruction_ptr $base [lindex $modes 0] 1 $value]
            incr instruction_ptr 2
        } elseif {$opcode == 4} {
            set p1 [get_param $mem $instruction_ptr $base [lindex $modes 0] 1]
            lappend output_list $p1
            incr instruction_ptr 2
            if {[llength $output_list] >= 3} {
                return "output_ready"
            }
        } elseif {$opcode in {5 6}} {
            set p1 [get_param $mem $instruction_ptr $base [lindex $modes 0] 1]
            set p2 [get_param $mem $instruction_ptr $base [lindex $modes 1] 2]
            if {($opcode == 5 && $p1 != 0) || ($opcode == 6 && $p1 == 0)} {
                set instruction_ptr $p2
            } else {
                incr instruction_ptr 3
            }
        } elseif {$opcode == 9} {
            set p1 [get_param $mem $instruction_ptr $base [lindex $modes 0] 1]
            incr base $p1
            incr instruction_ptr 2
        }
    }
}

set computers {}
set packet_queues {}
for {set i 0} {$i < 50} {incr i} {
    set mem [dict create]
    for {set j 0} {$j < [llength $program]} {incr j} {
        dict set mem $j [lindex $program $j]
    }
    lappend computers [list $mem 0 0 [list $i] {}]
    lappend packet_queues [list]
}

set first_packet_to_255 {}

while {true} {
    set idle 1
    for {set i 0} {$i < 50} {incr i} {
        lassign [lindex $computers $i] mem ip base inputs outputs
        set packet_queue [lindex $packet_queues $i]

        if {[llength $packet_queue] > 0} {
            lassign [lindex $packet_queue 0] x y
            set packet_queues [lreplace $packet_queues $i $i [lrange $packet_queue 1 end]]
            set inputs [concat $inputs [list $x $y]]
        } else {
            lappend inputs -1
        }

        set result [run_intcode mem ip base inputs outputs]
        lset computers $i [list $mem $ip $base $inputs $outputs]

        while {[llength $outputs] >= 3} {
            set idle 0
            lassign $outputs dest x y
            set outputs [lrange $outputs 3 end]
            lset computers $i [list $mem $ip $base $inputs $outputs]

            if {$dest == 255} {
                if {$first_packet_to_255 == {}} {
                    puts $y
                    exit
                }
            } elseif {$dest >= 0 && $dest < 50} {
                set target_queue [lindex $packet_queues $dest]
                lappend target_queue [list $x $y]
                lset packet_queues $dest $target_queue
            }
        }
    }
}