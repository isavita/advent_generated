
array set directions {
    N {0 -1}
    S {0 1}
    E {1 0}
    W {-1 0}
}
array set direction_turns {
    N {L W R E}
    S {L E R W}
    E {L N R S}
    W {L S R N}
}

proc read_grid {file_path} {
    set fd [open $file_path r]
    set grid [list]
    while {[gets $fd line] != -1} {
        set row [list]
        foreach char [split [string trim $line] ""] {
            lappend row $char
        }
        lappend grid $row
    }
    close $fd
    return $grid
}

proc dijkstra {grid part} {
    global directions direction_turns 

    set H [llength $grid]
    set W [llength [lindex $grid 0]]
    set end_x [expr {$W - 1}]
    set end_y [expr {$H - 1}]

    if {$part == 1} {
        set MAX_STEPS 3
        set MIN_STEPS_TURN 1
    } else { 
        set MAX_STEPS 10
        set MIN_STEPS_TURN 4
    }
    
    set pq [list]
    array set visited {}

    # State: {heat_loss x y dir steps}
    lappend pq [list 0 0 0 E 0]
    lappend pq [list 0 0 0 S 0]
    
    set pq [lsort -integer -index 0 $pq]

    while {[llength $pq] > 0} {
        set min_state [lindex $pq 0]
        set pq [lreplace $pq 0 0]
        
        lassign $min_state total_heat_loss x y direction steps

        if {$x == $end_x && $y == $end_y} {
            if {$part == 2 && $steps < $MIN_STEPS_TURN} {
                # Part 2 constraint: Must have taken minimum steps
            } else {
                return $total_heat_loss
            }
        }

        set state_key "$x,$y,$direction,$steps"
        
        if {[info exists visited($state_key)] && $visited($state_key) <= $total_heat_loss} {
            continue
        }

        set visited($state_key) $total_heat_loss

        set possible_moves [list]

        # Continue straight ('C')
        if {$steps < $MAX_STEPS} {
            lappend possible_moves [list C $direction]
        }

        # Turn Left/Right ('L', 'R')
        if {$steps >= $MIN_STEPS_TURN} {
            foreach turn {L R} {
                set turn_index [expr {2 * [lsearch {L R} $turn]} + 1]
                set new_direction [lindex $direction_turns($direction) $turn_index]
                lappend possible_moves [list $turn $new_direction]
            }
        }
        
        foreach move_data $possible_moves {
            lassign $move_data move new_direction
            
            lassign $directions($new_direction) dx dy
            set nx [expr {$x + $dx}]
            set ny [expr {$y + $dy}]

            if {$nx < 0 || $nx >= $W || $ny < 0 || $ny >= $H} {
                continue
            }

            set heat_gain [lindex [lindex $grid $ny] $nx]
            set new_heat_loss [expr {$total_heat_loss + $heat_gain}]

            if {$move == "C"} {
                set new_steps [expr {$steps + 1}]
            } else {
                set new_steps 1
            }
            
            set new_state [list $new_heat_loss $nx $ny $new_direction $new_steps]
            set new_state_key "$nx,$ny,$new_direction,$new_steps"

            if {[info exists visited($new_state_key)] && $visited($new_state_key) <= $new_heat_loss} {
                continue
            }

            lappend pq $new_state
            set pq [lsort -integer -index 0 $pq]
        }
    }

    return -1
}

proc main {} {
    set grid [read_grid "input.txt"]

    puts [dijkstra $grid 1]
    puts [dijkstra $grid 2]
}

main
