
#!/usr/bin/env tclsh

# Read input file
set input [open "input.txt" r]
set grid {}

# Parse input grid
while {[gets $input line] != -1} {
    set row {}
    foreach char [split $line ""] {
        lappend row [scan $char %d]
    }
    lappend grid $row
}
close $input

# Dijkstra's algorithm implementation
proc dijkstra {grid} {
    set rows [llength $grid]
    set cols [llength [lindex $grid 0]]
    
    # Initialize distance matrix
    set dist {}
    for {set i 0} {$i < $rows} {incr i} {
        set row {}
        for {set j 0} {$j < $cols} {incr j} {
            lappend row [expr {2147483647}]
        }
        lappend dist $row
    }
    
    # Set initial distance to 0
    lset dist 0 0 0
    
    # Priority queue simulation with a list
    set queue {{0 0 0}}
    
    # Directions
    set directions {{1 0} {0 1} {-1 0} {0 -1}}
    
    while {[llength $queue] > 0} {
        # Sort queue by risk (lowest risk first)
        set queue [lsort -index 2 -integer $queue]
        
        # Pop first element
        set curr [lindex $queue 0]
        set queue [lreplace $queue 0 0]
        
        lassign $curr x y risk
        
        # Reached bottom-right corner
        if {$x == $rows-1 && $y == $cols-1} {
            return $risk
        }
        
        # Explore neighbors
        foreach dir $directions {
            lassign $dir dx dy
            set nx [expr {$x + $dx}]
            set ny [expr {$y + $dy}]
            
            # Check bounds
            if {$nx >= 0 && $ny >= 0 && $nx < $rows && $ny < $cols} {
                set nextRisk [expr {$risk + [lindex [lindex $grid $nx] $ny]}]
                
                # Update if new path is shorter
                if {$nextRisk < [lindex [lindex $dist $nx] $ny]} {
                    lset dist $nx $ny $nextRisk
                    lappend queue [list $nx $ny $nextRisk]
                }
            }
        }
    }
    
    return -1
}

# Solve and print result
puts [dijkstra $grid]
