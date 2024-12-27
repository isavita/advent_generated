
#!/usr/bin/env tclsh

proc tilt_north {grid} {
    set rows [llength $grid]
    set cols [llength [lindex $grid 0]]
    
    for {set col 0} {$col < $cols} {incr col} {
        set next_empty 0
        for {set row 0} {$row < $rows} {incr row} {
            set cell [lindex [lindex $grid $row] $col]
            if {$cell eq "O"} {
                if {$row != $next_empty} {
                    lset grid $row $col "."
                    lset grid $next_empty $col "O"
                }
                incr next_empty
            } elseif {$cell eq "#"} {
                set next_empty [expr {$row + 1}]
            }
        }
    }
    return $grid
}

proc calculate_load {grid} {
    set rows [llength $grid]
    set total_load 0
    for {set row 0} {$row < $rows} {incr row} {
        set load [expr {$rows - $row}]
        foreach cell [lindex $grid $row] {
            if {$cell eq "O"} {
                incr total_load $load
            }
        }
    }
    return $total_load
}

# Read input from file
set file [open "input.txt" r]
set grid [list]
while {[gets $file line] != -1} {
    lappend grid [split $line ""]
}
close $file

# Tilt the grid north
set tilted_grid [tilt_north $grid]

# Calculate and print the total load
set total_load [calculate_load $tilted_grid]
puts $total_load
