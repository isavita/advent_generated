
#!/usr/bin/env tclsh

proc gcd {a b} {
    while {$b} {
        set t [expr {$a % $b}]
        set a $b
        set b $t
    }
    return $a
}

proc lcm {a b} {
    if {!$a || !$b} {return 0}
    expr {$a * $b / [gcd $a $b]}
}

set MAX_MODULES 64
set MAX_CONNECTIONS 16

set num_modules 0
array set modules {}
array set queue {}
set queue_head 0
set queue_tail 0
set queue_count 0

proc find_module_idx {name} {
    global num_modules modules
    for {set i 0} {$i < $num_modules} {incr i} {
        if {$modules($i,name) eq $name} {return $i}
    }
    return -1
}

proc add_module_definition {line} {
    global num_modules modules MAX_CONNECTIONS
    if {$num_modules >= $::MAX_MODULES} {error "Too many modules"}
    set modules($num_modules,type) ""
    set modules($num_modules,name) ""
    set modules($num_modules,num_outputs) 0
    set modules($num_modules,ff_state) 0
    set modules($num_modules,num_inputs) 0
    set modules($num_modules,input_last_pulse) ""
    set modules($num_modules,output_names) ""
    set modules($num_modules,output_indices) ""
    set modules($num_modules,input_indices) ""

    if {[regexp {^broadcaster -> (.+)$} $line -> outputs_part]} {
        set modules($num_modules,type) "BROADCASTER"
        set modules($num_modules,name) "broadcaster"
    } elseif {[regexp {^%([^ ]+) -> (.+)$} $line -> name_part outputs_part]} {
        set modules($num_modules,type) "FLIP_FLOP"
        set modules($num_modules,name) $name_part
    } elseif {[regexp {^&([^ ]+) -> (.+)$} $line -> name_part outputs_part]} {
        set modules($num_modules,type) "CONJUNCTION"
        set modules($num_modules,name) $name_part
    } else {error "Unknown module type in line: $line"}

    set outputs_list {}
    foreach token [split $outputs_part ", "] {
        if {$token ne ""} {lappend outputs_list $token}
    }
    set modules($num_modules,num_outputs) [llength $outputs_list]
    for {set i 0} {$i < $modules($num_modules,num_outputs)} {incr i} {
        set modules($num_modules,output_names,$i) [lindex $outputs_list $i]
    }
    incr num_modules
}

proc resolve_connections {} {
    global num_modules modules
    for {set i 0} {$i < $num_modules} {incr i} {
        for {set j 0} {$j < $modules($i,num_outputs)} {incr j} {
            set target_name $modules($i,output_names,$j)
            set target_idx [find_module_idx $target_name]
            set modules($i,output_indices,$j) $target_idx
            if {$target_idx != -1 && $modules($target_idx,type) eq "CONJUNCTION"} {
                set idx [set modules($target_idx,num_inputs)]
                set modules($target_idx,input_indices,$idx) $i
                set modules($target_idx,input_last_pulse,$idx) 0
                incr modules($target_idx,num_inputs)
            }
        }
    }
}

proc enqueue {from to pulse} {
    global queue queue_tail queue_count queue_head
    set queue($queue_tail,from) $from
    set queue($queue_tail,to) $to
    set queue($queue_tail,pulse) $pulse
    set queue_tail [expr {($queue_tail + 1) % 16384}]
    incr queue_count
}

proc dequeue {} {
    global queue queue_head queue_count
    set from $queue($queue_head,from)
    set to $queue($queue_head,to)
    set pulse $queue($queue_head,pulse)
    set queue_head [expr {($queue_head + 1) % 16384}]
    incr queue_count -1
    return [list $from $to $pulse]
}

set fp [open "input.txt" r]
while {[gets $fp line] >= 0} {
    set line [string trim $line]
    if {$line ne ""} {add_module_definition $line}
}
close $fp

resolve_connections
set broadcaster_idx [find_module_idx "broadcaster"]
if {$broadcaster_idx == -1} {error "Broadcaster module not found"}

set rx_feeder_idx -1
for {set i 0} {$i < $num_modules} {incr i} {
    for {set j 0} {$j < $modules($i,num_outputs)} {incr j} {
        if {$modules($i,output_names,$j) eq "rx"} {
            set rx_feeder_idx $i
            break
        }
    }
    if {$rx_feeder_idx != -1} {break}
}

if {$rx_feeder_idx == -1 || $modules($rx_feeder_idx,type) ne "CONJUNCTION"} {
    puts 0
    exit 0
}

set feeder_conj $rx_feeder_idx
set num_loops_to_find $modules($feeder_conj,num_inputs)
set loops_found 0
set press_count 0

for {set i 0} {$i < $num_loops_to_find} {incr i} {
    set loop_lengths($i) 0
}

while {$loops_found < $num_loops_to_find} {
    incr press_count
    enqueue -1 $broadcaster_idx 0

    while {$queue_count > 0} {
        lassign [dequeue] from_idx to_idx pulse
        if {$to_idx == $rx_feeder_idx && $pulse} {
            for {set i 0} {$i < $modules($feeder_conj,num_inputs)} {incr i} {
                if {$modules($feeder_conj,input_indices,$i) == $from_idx && $loop_lengths($i) == 0} {
                    set loop_lengths($i) $press_count
                    incr loops_found
                }
            }
        }
        if {$to_idx == -1} continue
        set target_module $to_idx
        set type $modules($target_module,type)
        if {$type eq "FLIP_FLOP"} {
            if {!$pulse} {
                set modules($target_module,ff_state) [expr {!$modules($target_module,ff_state)}]
                set pulse_to_send $modules($target_module,ff_state)
                for {set i 0} {$i < $modules($target_module,num_outputs)} {incr i} {
                    enqueue $target_module $modules($target_module,output_indices,$i) $pulse_to_send
                }
            }
        } elseif {$type eq "CONJUNCTION"} {
            set all_high 1
            for {set i 0} {$i < $modules($target_module,num_inputs)} {incr i} {
                if {$modules($target_module,input_indices,$i) == $from_idx} {
                    set modules($target_module,input_last_pulse,$i) $pulse
                }
                if {!$modules($target_module,input_last_pulse,$i)} {set all_high 0}
            }
            set pulse_to_send [expr {!$all_high}]
            for {set i 0} {$i < $modules($target_module,num_outputs)} {incr i} {
                enqueue $target_module $modules($target_module,output_indices,$i) $pulse_to_send
            }
        } else {
            set pulse_to_send $pulse
            for {set i 0} {$i < $modules($target_module,num_outputs)} {incr i} {
                enqueue $target_module $modules($target_module,output_indices,$i) $pulse_to_send
            }
        }
    }
    if {$loops_found == $num_loops_to_find} {break}
    if {$press_count > 1000000} {error "Exceeded 1,000,000 presses"}
}

set final_lcm 1
if {$loops_found == $num_loops_to_find} {
    set final_lcm $loop_lengths(0)
    for {set i 1} {$i < $num_loops_to_find} {incr i} {
        set final_lcm [lcm $final_lcm $loop_lengths($i)]
    }
} else {
    set final_lcm 0
}
puts $final_lcm
