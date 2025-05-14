
proc main {} {
    set filename "input.txt"
    set infinity 9999

    set flow_rates [dict create]
    set direct_tunnels [dict create]
    set all_valves [list]

    set file [open $filename r]
    while {[gets $file line] != -1} {
        regexp {Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)} $line -> valve_id flow_rate tunnels_str
        dict set flow_rates $valve_id $flow_rate
        lappend all_valves $valve_id

        set direct_conns [list]
        foreach tunnel [split $tunnels_str ", "] {
            lappend direct_conns $tunnel
        }
        dict set direct_tunnels $valve_id $direct_conns
    }
    close $file

    set distances [dict create]
    foreach v1 $all_valves {
        foreach v2 $all_valves {
            dict set distances $v1,$v2 $infinity
        }
        dict set distances $v1,$v1 0
    }
    foreach v [dict keys $direct_tunnels] {
        foreach dest [dict get $direct_tunnels $v] {
            dict set distances $v,$dest 1
        }
    }

    foreach k $all_valves {
        foreach i $all_valves {
            foreach j $all_valves {
                set dist_ik [dict get $distances $i,$k]
                set dist_kj [dict get $distances $k,$j]
                if {$dist_ik != $infinity && $dist_kj != $infinity} {
                    set current_dist [dict get $distances $i,$j]
                    if {$dist_ik + $dist_kj < $current_dist} {
                        dict set distances $i,$j [expr {$dist_ik + $dist_kj}]
                    }
                }
            }
        }
    }

    set high_flow_valves [list]
    foreach v $all_valves {
        if {[dict get $flow_rates $v] > 0} {
            lappend high_flow_valves $v
        }
    }

    set num_high_flow [llength $high_flow_valves]
    set max_total_pressure 0

    for {set i 0} {$i < [expr {1 << $num_high_flow}]} {incr i} {
        set my_valves [list]
        set elephant_valves [list]

        for {set j 0} {$j < $num_high_flow} {incr j} {
            if {$i & [expr {1 << $j}]} {
                lappend my_valves [lindex $high_flow_valves $j]
            } else {
                lappend elephant_valves [lindex $high_flow_valves $j]
            }
        }

        if {[llength $my_valves] == 0 || [llength $elephant_valves] == 0} {
            continue
        }

        set memo1 [dict create]
        set pressure1 [solve "AA" 26 $my_valves $distances $flow_rates memo1]

        set memo2 [dict create]
        set pressure2 [solve "AA" 26 $elephant_valves $distances $flow_rates memo2]

        set total_pressure [expr {$pressure1 + $pressure2}]

        if {$total_pressure > $max_total_pressure} {
            set max_total_pressure $total_pressure
        }
    }

    puts $max_total_pressure
}

proc solve {current_valve time_left remaining_valves distances flow_rates memo_dict} {
    upvar 1 $memo_dict memo

    set sorted_remaining [lsort $remaining_valves]
    set key "$current_valve,$time_left,[join $sorted_remaining ,]"

    if {[dict exists $memo $key]} {
        return [dict get $memo $key]
    }

    set max_p 0

    foreach next_valve $remaining_valves {
        set dist [dict get $distances $current_valve,$next_valve]
        set time_to_reach_and_open [expr {$dist + 1}]
        set new_time_left [expr {$time_left - $time_to_reach_and_open}]

        if {$new_time_left > 0} {
            set flow_rate [dict get $flow_rates $next_valve]
            set pressure_gained_now [expr {$new_time_left * $flow_rate}]

            set new_remaining_valves [lsearch -all -inline -not $remaining_valves $next_valve]
            set pressure_from_next [solve $next_valve $new_time_left $new_remaining_valves $distances $flow_rates memo]

            set total_pressure_this_path [expr {$pressure_gained_now + $pressure_from_next}]

            if {$total_pressure_this_path > $max_p} {
                set max_p $total_pressure_this_path
            }
        }
    }

    dict set memo $key $max_p
    return $max_p
}

main
