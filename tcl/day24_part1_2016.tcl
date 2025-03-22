
# Optimized Tcl solution for Advent of Code Day 24

proc calculate_distances {grid start} {
    set distances [dict create]
    set queue [list [list $start 0]]
    set visited [dict create $start 1]

    while {[llength $queue] > 0} {
        set current [lindex $queue 0]
        set queue [lrange $queue 1 end]

        set pos [lindex $current 0]
        set dist [lindex $current 1]
        set x [lindex $pos 0]
        set y [lindex $pos 1]
        
        set char_at_pos [lindex [lindex $grid $y] $x]

        if {$char_at_pos ne "." && $char_at_pos ne "#" && [string is integer -strict $char_at_pos]} {
            dict set distances $char_at_pos $dist
        }

        foreach {dx dy} {0 1  0 -1  1 0  -1 0} {
            set nx [expr {$x + $dx}]
            set ny [expr {$y + $dy}]
            
             if {$nx >= 0 && $nx < [llength [lindex $grid 0]] && $ny >= 0 && $ny < [llength $grid]} {
                set next_char [lindex [lindex $grid $ny] $nx]
                if {$next_char ne "#" && ![dict exists $visited [list $nx $ny]]} {
                   
                    lappend queue [list [list $nx $ny] [expr {$dist + 1}]]
                    dict set visited [list $nx $ny] 1
                }
            }
        }
    }
    return $distances
}


proc find_shortest_path {grid} {
    set locations [dict create]
    set start_pos ""

     # Find locations of numbers
    for {set y 0} {$y < [llength $grid]} {incr y} {
        for {set x 0} {$x < [llength [lindex $grid $y]]} {incr x} {
            set char [lindex [lindex $grid $y] $x]
            if {$char ne "." && $char ne "#" && [string is integer -strict $char]} {
                if {$char eq "0"} {
                    set start_pos [list $x $y]
                }
                dict set locations $char [list $x $y]
            }
        }
    }
     # Pre-calculate all pairwise distances
    set all_distances [dict create]
    dict for {num pos} $locations {
        dict set all_distances $num [calculate_distances $grid $pos]
    }

    set min_dist [expr {1 << 30}]  ;# A large number
    set other_locations [dict keys $locations]
    set other_locations [lsort -dictionary [lsearch -all -inline -not $other_locations 0]]

    #Using permutations efficiently.  Requires Tcl 8.6+
    package require struct::list
    
    foreach perm [struct::list permutations $other_locations] {
        set current_dist 0
        set current_loc "0"
       
        foreach next_loc $perm {
             set distances_from_current [dict get $all_distances $current_loc]
             set dist_to_next [dict get $distances_from_current $next_loc]

            set current_dist [expr {$current_dist + $dist_to_next}]
            set current_loc $next_loc
        }

        if {$current_dist < $min_dist} {
            set min_dist $current_dist
        }
    }

    return $min_dist
}

proc main {} {
    if {[file exists "input.txt"]} {
        set f [open "input.txt" r]
        set grid [list]
        while {[gets $f line] != -1} {
           lappend grid [split $line ""]
        }
        close $f

        set shortest_path [find_shortest_path $grid]
        puts "Shortest path: $shortest_path"

    } else {
        puts "Error: input.txt not found."
    }
}

main
