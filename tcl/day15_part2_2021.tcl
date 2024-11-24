
#!/usr/bin/env tclsh

# Read input file
set fp [open "input.txt" r]
set initialGrid {}
while {[gets $fp line] >= 0} {
    set row {}
    foreach char [split $line ""] {
        lappend row [scan $char %d]
    }
    lappend initialGrid $row
}
close $fp

# Extend grid function
proc extendGrid {initialGrid} {
    set rows [llength $initialGrid]
    set cols [llength [lindex $initialGrid 0]]
    set extendedGrid {}

    for {set i 0} {$i < $rows * 5} {incr i} {
        set newRow {}
        for {set j 0} {$j < $cols * 5} {incr j} {
            set newRisk [expr {[lindex $initialGrid [expr {$i % $rows}] [expr {$j % $cols}]] + $i / $rows + $j / $cols}]
            if {$newRisk > 9} {
                set newRisk [expr {$newRisk - 9}]
            }
            lappend newRow $newRisk
        }
        lappend extendedGrid $newRow
    }
    return $extendedGrid
}

# Priority queue implementation
proc pqInit {} {
    return {}
}

proc pqPush {pqVar x} {
    upvar $pqVar pq
    lappend pq $x
    set pq [lsort -index 2 -integer $pq]
}

proc pqPop {pqVar} {
    upvar $pqVar pq
    set item [lindex $pq 0]
    set pq [lrange $pq 1 end]
    return $item
}

# Dijkstra's algorithm
proc dijkstra {grid} {
    set rows [llength $grid]
    set cols [llength [lindex $grid 0]]
    
    # Initialize distance matrix
    set dist {}
    for {set i 0} {$i < $rows} {incr i} {
        set rowDist {}
        for {set j 0} {$j < $cols} {incr j} {
            lappend rowDist [expr {2147483647}]
        }
        lappend dist $rowDist
    }
    
    # Set first cell distance to 0
    lset dist 0 0 0
    
    # Priority queue
    set pq [pqInit]
    pqPush pq [list 0 0 0]
    
    # Directions
    set directions {{1 0} {0 1} {-1 0} {0 -1}}
    
    while {[llength $pq] > 0} {
        set curr [pqPop pq]
        lassign $curr x y risk
        
        # Reached bottom-right
        if {$x == $rows-1 && $y == $cols-1} {
            return $risk
        }
        
        # Check adjacent cells
        foreach dir $directions {
            lassign $dir dx dy
            set nx [expr {$x + $dx}]
            set ny [expr {$y + $dy}]
            
            # Check bounds
            if {$nx >= 0 && $ny >= 0 && $nx < $rows && $ny < $cols} {
                set nextRisk [expr {$risk + [lindex $grid $nx $ny]}]
                
                # Update if shorter path found
                if {$nextRisk < [lindex $dist $nx $ny]} {
                    lset dist $nx $ny $nextRisk
                    pqPush pq [list $nx $ny $nextRisk]
                }
            }
        }
    }
    
    return -1
}

# Main execution
set extendedGrid [extendGrid $initialGrid]
puts [dijkstra $extendedGrid]
