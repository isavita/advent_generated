
proc main {} {
    set garden [dict create]
    set start ""
    set max_size 0
    set fileId [open "input.txt" r]
    set y 0
    while {[gets $fileId line] != -1} {
        set max_x [string length $line]
        for {set x 0} {$x < $max_x} {incr x} {
            set char [string index $line $x]
            if {$char ne "#"} {
                dict set garden "$x,$y" 1
            }
            if {$char eq "S"} {
                set start "$x,$y"
            }
        }
        incr y
    }
    close $fileId
    set max_size $y

    set current_queue [dict create $start 1]
    set done [list]
    set num_iterations 26501365
    set base_step [expr {($max_size - 1) / 2}]

    set target_steps [list $base_step [expr {$base_step + $max_size}] [expr {$base_step + 2 * $max_size}]]
    set max_sim_steps [lindex $target_steps end]

    for {set i 0} {$i <= $max_sim_steps} {incr i} {
        if {[lsearch $target_steps $i] != -1} {
            lappend done [dict size $current_queue]
        }

        if {$i == $max_sim_steps} {
            break
        }

        set next_queue [dict create]
        foreach point [dict keys $current_queue] {
            lassign [split $point ,] cur_x cur_y
            foreach dir { {0 1} {0 -1} {1 0} {-1 0} } {
                lassign $dir dx dy
                set next_x [expr {$cur_x + $dx}]
                set next_y [expr {$cur_y + $dy}]

                set wrapped_x [expr {(int($next_x) % $max_size + $max_size) % $max_size}]
                set wrapped_y [expr {(int($next_y) % $max_size + $max_size) % $max_size}]

                if {[dict exists $garden "$wrapped_x,$wrapped_y"]} {
                    dict set next_queue "$next_x,$next_y" 1
                }
            }
        }
        set current_queue $next_queue
    }

    set n_val [expr {$num_iterations / $max_size}]
    set result [quadratic_function $n_val [lindex $done 0] [lindex $done 1] [lindex $done 2]]
    puts $result
}

proc quadratic_function {n y0 y1 y2} {
    set val_A [expr {$y2 - 2 * $y1 + $y0}]
    set A [expr {int(floor(double($val_A) / 2.0))}]

    set B [expr {($y1 - $y0) - $A}]
    set C $y0

    set res [expr {$A * $n * $n + $B * $n + $C}]
    return $res
}

main
