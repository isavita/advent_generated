
#!/usr/bin/env tclsh

# Helper function to read the input file into a list of lists
proc read_input_file {filename} {
    set file [open $filename r]
    set data {}
    while {[gets $file line] >= 0} {
        lappend data [split $line ""]
    }
    close $file
    return $data
}

# Helper function to perform a breadth-first search to find a region
proc bfs {grid row col char visited} {
    set rows [llength $grid]
    set cols [llength [lindex $grid 0]]
    set queue [list [list $row $col]]
    set area 0
    set perimeter 0
    set visited_nodes {}

    while {[llength $queue] > 0} {
        set current [lindex $queue 0]
        set queue [lrange $queue 1 end]
        set r [lindex $current 0]
        set c [lindex $current 1]

        if {[dict exists $visited $r,$c]} {
            continue
        }

        dict set visited $r,$c 1
        lappend visited_nodes [list $r $c]
        incr area

        # Check neighbors for perimeter
        foreach {dr dc} {0 1 0 -1 1 0 -1 0} {
            set nr [expr {$r + $dr}]
            set nc [expr {$c + $dc}]

            if {$nr < 0 || $nr >= $rows || $nc < 0 || $nc >= $cols} {
                incr perimeter
            } elseif {[lindex [lindex $grid $nr] $nc] ne $char} {
                incr perimeter
            }
        }

        # Add valid neighbors to the queue
        foreach {dr dc} {0 1 0 -1 1 0 -1 0} {
            set nr [expr {$r + $dr}]
            set nc [expr {$c + $dc}]

            if {$nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols && \
                [lindex [lindex $grid $nr] $nc] eq $char && \
                ![dict exists $visited $nr,$nc]} {
                lappend queue [list $nr $nc]
            }
        }
    }
    return [list $area $perimeter $visited]
}

# Main function to calculate the total fencing price
proc calculate_total_price {grid} {
    set rows [llength $grid]
    set cols [llength [lindex $grid 0]]
    set visited [dict create]
    set total_price 0

    for {set r 0} {$r < $rows} {incr r} {
        for {set c 0} {$c < $cols} {incr c} {
            if {![dict exists $visited $r,$c]} {
                set char [lindex [lindex $grid $r] $c]
                set result [bfs $grid $r $c $char $visited]
                set area [lindex $result 0]
                set perimeter [lindex $result 1]
                set visited [lindex $result 2]
                incr total_price [expr {$area * $perimeter}]
            }
        }
    }
    return $total_price
}

# Main entry point
if {![info exists argv0]} {
    set argv0 [info script]
}

if {$argc != 0} {
    puts "Usage: $argv0"
    exit 1
}

set grid [read_input_file "input.txt"]
set total_price [calculate_total_price $grid]
puts $total_price
