
set grid {}
set keys {}
set doors {}
set all_keys {}
set robot_positions {}
set key_positions {}
set key_graph {}

proc bitmask_add_char {mask char} {
    global all_keys
    set index [lsearch -sorted $all_keys $char]
    if {$index == -1} {
        error "Character '$char' not found in all_keys"
    }
    return [expr {$mask | (1 << $index)}]
}

proc bitmask_has_mask {haystack_mask needle_mask} {
    return [expr {($haystack_mask & $needle_mask) == $needle_mask}]
}

proc bitmask_count {mask} {
    set count 0
    for {set i 0} {$i < 26} {incr i} {
        if {$mask & (1 << $i)} {
            incr count
        }
    }
    return $count
}

proc bfs_precompute {start_pos start_node_name} {
    global grid keys doors all_keys

    set queue [list [list $start_pos 0 0]]
    set visited [dict create]
    set results [dict create]

    set start_char ""
    if {![string match "@*" $start_node_name]} {
        set start_char $start_node_name
    }

    while {[llength $queue] > 0} {
        set current [lindex $queue 0]
        set queue [lrange $queue 1 end]

        set pos [lindex $current 0]
        set x [lindex $pos 0]
        set y [lindex $pos 1]
        set dist [lindex $current 1]
        set required_keys_bitmask [lindex $current 2]

        set visit_key "[list $x $y] $required_keys_bitmask"
        if {[dict exists $visited $visit_key]} {
            continue
        }
        dict set visited $visit_key 1

        set cell [lindex $grid $y $x]

        if {[string is lower $cell] && ($cell ne $start_char) && ! [dict exists $results $cell]} {
             dict set results $cell [list $dist $required_keys_bitmask]
        }

        foreach delta {{0 -1} {0 1} {-1 0} {1 0}} {
            set dx [lindex $delta 0]
            set dy [lindex $delta 1]
            set nx [expr {$x + $dx}]
            set ny [expr {$y + $dy}]

            if {$ny >= 0 && $ny < [llength $grid] && $nx >= 0 && $nx < [llength [lindex $grid $ny]]} {
                set ncell [lindex $grid $ny $nx]

                if {$ncell eq "#"} { continue }

                set next_required_keys_bitmask $required_keys_bitmask

                if {[string is upper $ncell]} {
                    set key_char [string tolower $ncell]
                    set next_required_keys_bitmask [bitmask_add_char $next_required_keys_bitmask $key_char]
                }

                set next_visit_key "[list $nx $ny] $next_required_keys_bitmask"
                if {![dict exists $visited $next_visit_key]} {
                    lappend queue [list [list $nx $ny] [expr {$dist + 1}] $next_required_keys_bitmask]
                }
            }
        }
    }
    return $results
}

proc dijkstra {} {
    global key_positions all_keys key_graph robot_positions keys

    set total_keys [llength $all_keys]
    set initial_positions [list]
    for {set i 0} {$i < [llength $robot_positions]} {incr i} {
        lappend initial_positions "@$i"
    }

    set pq [dict create]
    dict set pq 0 [list [list $initial_positions 0]]

    set visited [dict create]
    dict set visited "[list $initial_positions] 0" 0

    while {[dict size $pq] > 0} {
        set costs [lsort -integer [dict keys $pq]]
        set current_cost [lindex $costs 0]
        set states_at_cost [dict get $pq $current_cost]

        set current_state [lindex $states_at_cost 0]
        set states_at_cost [lrange $states_at_cost 1 end]

        if {[llength $states_at_cost] == 0} {
            dict unset pq $current_cost
        } else {
            dict set pq $current_cost $states_at_cost
        }

        set current_positions [lindex $current_state 0]
        set current_collected_mask [lindex $current_state 1]
        set current_state_key "[list $current_positions] $current_collected_mask"

        if {[dict exists $visited $current_state_key] && [dict get $visited $current_state_key] < $current_cost} {
            continue
        }

        if {[bitmask_count $current_collected_mask] == $total_keys} {
             return $current_cost
        }

        for {set i 0} {$i < [llength $current_positions]} {incr i} {
            set robot_pos_node [lindex $current_positions $i]

            if {! [dict exists $key_graph $robot_pos_node]} { continue }
            set reachable_from_robot [dict get $key_graph $robot_pos_node]

            dict for {target_key dist_info} $reachable_from_robot {
                set dist [lindex $dist_info 0]
                set required_mask_for_path [lindex $dist_info 1]

                set target_key_mask [bitmask_add_char 0 $target_key]

                if {![bitmask_has_mask $current_collected_mask $target_key_mask] &&
                    [bitmask_has_mask $current_collected_mask $required_mask_for_path]} {

                    set new_collected_mask [expr {$current_collected_mask | $target_key_mask}]
                    set new_positions [lreplace $current_positions $i $i $target_key]
                    set new_cost [expr {$current_cost + $dist}]
                    set new_state_key "[list $new_positions] $new_collected_mask"

                    if {![dict exists $visited $new_state_key] || [dict get $visited $new_state_key] > $new_cost} {
                        dict set visited $new_state_key $new_cost
                        if {![dict exists $pq $new_cost]} {
                            dict set pq $new_cost [list]
                        }
                        dict lappend pq $new_cost [list $new_positions $new_collected_mask]
                    }
                }
            }
        }
    }
    return -1
}

proc main {} {
    global grid keys doors all_keys robot_positions key_positions key_graph

    set f [open "input.txt" r]
    while {[gets $f line] != -1} {
        if {[string trim $line] ne ""} {
            lappend grid [split [string trim $line "\n"] ""]
        }
    }
    close $f

    set found false
    for {set y 1} {$y < [expr {[llength $grid] - 1}]} {incr y} {
        for {set x 1} {$x < [expr {[llength [lindex $grid $y]] - 1}]} {incr x} {
            if {[lindex $grid $y $x] eq "@"} {
                if {[lindex $grid [expr {$y-1}] $x] eq "." &&
                    [lindex $grid [expr {$y+1}] $x] eq "." &&
                    [lindex $grid $y [expr {$x-1}]] eq "." &&
                    [lindex $grid $y [expr {$x+1}]] eq "."} {

                    set row_ym1 [lindex $grid [expr {$y-1}]]
                    set row_y   [lindex $grid $y]
                    set row_yp1 [lindex $grid [expr {$y+1}]]

                    set new_row_ym1 [lreplace $row_ym1 [expr {$x-1}] [expr {$x+1}] "@" "#" "@"]
                    lset grid [expr {$y-1}] $new_row_ym1

                    set new_row_y [lreplace $row_y [expr {$x-1}] [expr {$x+1}] "#" "#" "#"]
                    lset grid $y $new_row_y

                    set new_row_yp1 [lreplace $row_yp1 [expr {$x-1}] [expr {$x+1}] "@" "#" "@"]
                    lset grid [expr {$y+1}] $new_row_yp1

                    set found true
                    break
                }
            }
        }
        if {$found} { break }
    }

    if {!$found} {
        puts "Error: Could not find the '@' symbol surrounded by open spaces."
        exit 1
    }

    set robot_positions {}
    for {set y 0} {$y < [llength $grid]} {incr y} {
        for {set x 0} {$x < [llength [lindex $grid $y]]} {incr x} {
            if {[lindex $grid $y $x] eq "@"} {
                lappend robot_positions [list $x $y]
            }
        }
    }

    set keys {}
    set doors {}
    set all_keys {}
    for {set y 0} {$y < [llength $grid]} {incr y} {
        for {set x 0} {$x < [llength [lindex $grid $y]]} {incr x} {
            set cell [lindex $grid $y $x]
            if {[string is lower $cell]} {
                dict set keys $cell [list $x $y]
                lappend all_keys $cell
            } elseif {[string is upper $cell]} {
                dict set doors $cell [list $x $y]
            }
        }
    }
    set all_keys [lsort $all_keys]

    set key_positions {}
    dict append key_positions $keys
    for {set i 0} {$i < [llength $robot_positions]} {incr i} {
        dict set key_positions "@$i" [lindex $robot_positions $i]
    }

    set key_graph {}
    set all_nodes [dict keys $keys]
    for {set i 0} {$i < [llength $robot_positions]} {incr i} {
        lappend all_nodes "@$i"
    }

    foreach node $all_nodes {
        set pos {}
        if {[string match "@*" $node]} {
            set index [string range $node 1 end]
            set pos [lindex $robot_positions $index]
        } else {
            set pos [dict get $keys $node]
        }
        dict set key_graph $node [bfs_precompute $pos $node]
    }

    set result [dijkstra]
    puts $result
}

main
