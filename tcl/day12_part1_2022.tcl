
#!/usr/bin/env tclsh

proc readGrid {} {
    set grid [dict create]
    set start {}
    set end {}
    set as {}
    set y 0

    set file [open "input.txt" r]
    while {[gets $file line] != -1} {
        set chars [split $line ""]
        for {set x 0} {$x < [llength $chars]} {incr x} {
            set char [lindex $chars $x]
            dict set grid "$x,$y" $char
            
            if {$char eq "S"} {
                set start "$x,$y"
            } elseif {$char eq "E"} {
                set end "$x,$y"
            } elseif {$char eq "a"} {
                lappend as "$x,$y"
            }
        }
        incr y
    }
    close $file

    dict set grid $start "a"
    dict set grid $end "z"

    return [list $grid $start $end $as]
}

proc getNeighbors {point} {
    lassign [split $point ","] x y
    return [list [list [expr {$x+1}] $y] [list [expr {$x-1}] $y] \
                 [list $x [expr {$y+1}]] [list $x [expr {$y-1}]]]
}

proc dijkstra {grid end} {
    set dist [dict create $end 0]
    set queue [list [list $end 0]]

    while {[llength $queue] > 0} {
        set queue [lsort -integer -index 1 $queue]
        set current [lindex $queue 0]
        set queue [lrange $queue 1 end]

        lassign $current curr_point curr_dist
        
        foreach neighbor [getNeighbors $curr_point] {
            set nx [lindex $neighbor 0]
            set ny [lindex $neighbor 1]
            set next_point "$nx,$ny"

            if {![dict exists $grid $next_point]} continue

            set curr_height [scan [dict get $grid $curr_point] %c]
            set next_height [scan [dict get $grid $next_point] %c]

            if {$curr_height - $next_height > 1} continue

            set next_dist [expr {$curr_dist + 1}]

            if {![dict exists $dist $next_point] || $next_dist < [dict get $dist $next_point]} {
                dict set dist $next_point $next_dist
                lappend queue [list $next_point $next_dist]
            }
        }
    }

    return $dist
}

proc main {} {
    lassign [readGrid] grid start end as

    set dists [dijkstra $grid $end]
    set path_length [dict get $dists $start]

    puts $path_length
}

main
