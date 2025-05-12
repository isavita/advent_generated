
# Define directions
set North [list 0 -1]
set West  [list -1 0]
set South [list 0 1]
set East  [list 1 0]
set Directions [list $North $West $South $East]

proc add_coord {c1 c2} {
    lassign $c1 x1 y1
    lassign $c2 x2 y2
    return [list [expr {$x1 + $x2}] [expr {$y1 + $y2}]]
}

proc subtract_coord {c1 c2} {
    lassign $c1 x1 y1
    lassign $c2 x2 y2
    return [list [expr {$x1 - $x2}] [expr {$y1 - $y2}]]
}

proc opposite_coord {c} {
    lassign $c x y
    return [list [expr {-$x}] [expr {-$y}]]
}

proc coords_equal {c1 c2} {
    return [expr {[lindex $c1 0] == [lindex $c2 0] && [lindex $c1 1] == [lindex $c2 1]}]
}

proc info_to_string {info} {
    lassign $info coord dir numStraight
    lassign $coord x y
    lassign $dir dx dy
    return "$x,$y,$dx,$dy,$numStraight"
}

proc string_to_info {info_str} {
    regexp {(-?\d+),(-?\d+),(-?\d+),(-?\d+),(\d+)} $info_str -> x y dx dy numStraight
    set coord [list $x $y]
    set dir [list $dx $dy]
    return [list $coord $dir $numStraight]
}

proc isInBounds {coord grid_array_name} {
    upvar 1 $grid_array_name grid
    lassign $coord x y
    return [expr {$x >= 0 && $x < $grid(width) && $y >= 0 && $y < $grid(height)}]
}

proc buildGrid {lines} {
    array set grid {}
    set grid(height) [llength $lines]
    set grid(width) [string length [lindex $lines 0]]
    set y 0
    foreach line $lines {
        for {set x 0} {$x < $grid(width)} {incr x} {
            set grid(data,$y,$x) [string index $line $x]
        }
        incr y
    }
    return [array get grid]
}

proc neighbors4 {coord grid_array_name} {
    upvar 1 $grid_array_name grid

    set neighbors [list]
    foreach dir $::Directions {
        set neighbor [add_coord $coord $dir]
        if {[isInBounds $neighbor grid]} {
            lappend neighbors $neighbor
        }
    }
    return $neighbors
}

proc heuristic {c1 c2} {
    lassign $c1 x1 y1
    lassign $c2 x2 y2
    return [expr {abs($x1 - $x2) + abs($y1 - $y2)}]
}

proc pq_create {} {
    return [list]
}

proc pq_enqueue {pq_var item priority} {
    upvar 1 $pq_var pq

    set entry [list $priority $item]

    set idx -1
    for {set i 0} {$i < [llength $pq]} {incr i} {
        set current_priority [lindex [lindex $pq $i] 0]
        if {$priority < $current_priority} {
            set idx $i
            break
        }
    }

    if {$idx == -1} {
        lappend pq $entry
    } else {
        set pq [linsert $pq $idx $entry]
    }
}

proc pq_dequeue {pq_var} {
    upvar 1 $pq_var pq

    if {[llength $pq] == 0} {
        return ""
    }

    set entry [lindex $pq 0]
    set pq [lreplace $pq 0 0]

    return [lindex $entry 1]
}

proc pq_is_empty {pq} {
    return [expr {[llength $pq] == 0}]
}

proc aStarConstrained {grid_array_name start goal minStraight maxStraight} {
    upvar 1 $grid_array_name grid

    array set costSoFar {}

    set start_coord $start
    set start_dir [list 0 0]
    set start_numStraight 0
    set start_info [list $start_coord $start_dir $start_numStraight]
    set start_info_str [info_to_string $start_info]

    set frontier [pq_create]
    pq_enqueue frontier $start_info_str 0

    set costSoFar($start_info_str) 0

    while {![pq_is_empty $frontier]} {
        set current_info_str [pq_dequeue frontier]

        if {![info exists costSoFar($current_info_str)]} {
            continue
        }
        set current_cost $costSoFar($current_info_str)

        set current_info [string_to_info $current_info_str]
        lassign $current_info current_coord current_dir current_numStraight

        if {[coords_equal $current_coord $goal]} {
             return $current_cost
        }

        foreach dir_into_next $::Directions {
            set next_coord [add_coord $current_coord $dir_into_next]

            if {![isInBounds $next_coord grid]} {
                continue
            }

            set opp_dir [opposite_coord $current_dir]
            if {[coords_equal $dir_into_next $opp_dir] && ![coords_equal $current_dir [list 0 0]]} {
                 continue
            }

            set is_moving_straight_relative [coords_equal $dir_into_next $current_dir]

            set new_numStraight 1
            if {$is_moving_straight_relative} {
                 set new_numStraight [expr {$current_numStraight + 1}]
            }

            set next_info_str [info_to_string [list $next_coord $dir_into_next $new_numStraight]]

            set cost_to_next $grid(data,[lindex $next_coord 1],[lindex $next_coord 0])
            set new_cost [expr {$current_cost + $cost_to_next}]

            set actual_cost ""
            if {[info exists costSoFar($next_info_str)]} {
                set actual_cost $costSoFar($next_info_str)
            }

            set isLowerCost [expr {$actual_cost eq "" || $new_cost < $actual_cost}]

            set passes_straight_constraints false
            if {$is_moving_straight_relative} {
                if {$new_numStraight <= $maxStraight} {
                    set passes_straight_constraints true
                }
            } else {
                 if {($current_numStraight >= $minStraight || [coords_equal $current_coord $start]) && $new_numStraight <= $maxStraight} {
                    set passes_straight_constraints true
                 }
            }

            if {$isLowerCost && $passes_straight_constraints} {
                set costSoFar($next_info_str) $new_cost
                set priority [expr {$new_cost + [heuristic $next_coord $goal]}]
                pq_enqueue frontier $next_info_str $priority
            }
        }
    }

    return -1
}

proc solve {lines} {
    array set grid_arr [buildGrid $lines]
    set start [list 0 0]
    set goal [list [expr {$grid_arr(width) - 1}] [expr {$grid_arr(height) - 1}]]
    return [aStarConstrained grid_arr $start $goal 0 3]
}

proc main {} {
    set filename "input.txt"
    set file [open $filename r]
    set content [read $file]
    close $file
    set lines [split [string trim $content] "\n"]

    puts [solve $lines]
}

main
