
# --- Day 7: The Sum of Its Parts ---

# Function to parse dependencies from input lines
proc parse_dependencies {lines} {
    set dependencies [dict create]
    set all_steps [list]

    foreach line $lines {
        # Use regexp to extract the before and after steps
        if {[regexp {Step (.) must be finished before step (.) can begin\.} $line -> before after]} {
            if {![dict exists $dependencies $after]} {
                dict set dependencies $after [list]
            }
            dict lappend dependencies $after $before

            if {[lsearch -exact $all_steps $before] == -1} {
                lappend all_steps $before
            }
            if {[lsearch -exact $all_steps $after] == -1} {
                lappend all_steps $after
            }
        }
    }
    return [list $dependencies $all_steps]
}

# Function to solve Part 1
proc solve_part1 {dependencies all_steps} {
    set completed [list]
    set available [list]
    set result ""

    # Find initial available steps (those with no dependencies)
    foreach step $all_steps {
        if {![dict exists $dependencies $step]} {
            lappend available $step
        }
    }
    set available [lsort $available]


    while {[llength $available] > 0} {
        set current_step [lindex $available 0]
        set available [lreplace $available 0 0]
        lappend completed $current_step
        append result $current_step
        
        # Find newly available steps
        foreach step $all_steps {
            if {[lsearch -exact $completed $step] == -1 && [lsearch -exact $available $step] == -1} {
                set can_start 1
                if {[dict exists $dependencies $step]} {
                    foreach dep [dict get $dependencies $step] {
                        if {[lsearch -exact $completed $dep] == -1} {
                            set can_start 0
                            break
                        }
                    }
                }
                if {$can_start} {
                   lappend available $step
                }
            }
        }
         set available [lsort $available]
    }
    
    return $result
}
# Function to solve Part 2
proc solve_part2 {dependencies all_steps num_workers step_duration_offset} {
    set completed [list]
    set available [list]
    set in_progress [dict create]
    set time 0

    # Find initial available steps
   foreach step $all_steps {
        if {![dict exists $dependencies $step]} {
            lappend available $step
        }
    }
    set available [lsort $available]

    while { [llength $completed] != [llength $all_steps] } {
        # Assign available steps to workers
        while {[llength $available] > 0 && [dict size $in_progress] < $num_workers} {
            set step [lindex $available 0]
            set available [lreplace $available 0 0]
            dict set in_progress $step [expr {$step_duration_offset + [scan $step %c] - [scan A %c] + 1}]
        }
        #puts "Time: $time, In Progress: $in_progress, Available: $available, Completed $completed"
        # Simulate a time step (decrement time for in-progress steps)

        set completed_this_step [list]
        set new_in_progress [dict create]
        dict for {step duration} $in_progress {
            set new_duration [expr {$duration - 1}]
            if {$new_duration == 0} {
                lappend completed_this_step $step
            } else {
                dict set new_in_progress $step $new_duration
            }
        }
        set in_progress $new_in_progress

        foreach step $completed_this_step {
            lappend completed $step
           # Find newly available steps
            foreach next_step $all_steps {
                if {[lsearch -exact $completed $next_step] == -1 && [lsearch -exact $available $next_step] == -1 && ![dict exists $in_progress $next_step]} {
                    set can_start 1
                     if {[dict exists $dependencies $next_step]} {
                        foreach dep [dict get $dependencies $next_step] {
                            if {[lsearch -exact $completed $dep] == -1} {
                                set can_start 0
                                break
                            }
                        }
                    }
                    if {$can_start} {
                        lappend available $next_step
                    }
                }
            }
            set available [lsort $available]
        }

        incr time
    }
    return $time
}


# Main function
proc main {} {
    # Read input from file
    set f [open "input.txt" r]
    set lines [split [read $f] "\n"]
    close $f

    # Parse dependencies and solve
    lassign [parse_dependencies $lines] dependencies all_steps
     set part1_result [solve_part1 $dependencies $all_steps]
    puts "Part 1: $part1_result"

    set num_workers 5
    set step_duration_offset 60
    set part2_result [solve_part2 $dependencies $all_steps $num_workers $step_duration_offset]
    puts "Part 2: $part2_result"
}

# Call the main function
main
