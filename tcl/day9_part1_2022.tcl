
#!/usr/bin/env tclsh

# Read input file
set file [open "input.txt" r]
set content [read $file]
close $file

# Initialize variables
set head {0 0}
set tail {0 0}
set visited [dict create {0 0} 1]

# Abs function
proc abs {x} {
    return [expr {abs($x)}]
}

# Process each line
foreach line [split $content "\n"] {
    if {$line eq ""} continue
    
    lassign [split $line " "] dir steps
    
    for {set i 0} {$i < $steps} {incr i} {
        # Move head
        switch $dir {
            R { lset head 0 [expr {[lindex $head 0] + 1}] }
            L { lset head 0 [expr {[lindex $head 0] - 1}] }
            U { lset head 1 [expr {[lindex $head 1] + 1}] }
            D { lset head 1 [expr {[lindex $head 1] - 1}] }
        }
        
        # Check tail movement
        set dx [expr {[lindex $head 0] - [lindex $tail 0]}]
        set dy [expr {[lindex $head 1] - [lindex $tail 1]}]
        
        if {[abs $dx] > 1 || [abs $dy] > 1} {
            # Diagonal move
            if {$dx != 0} {
                lset tail 0 [expr {[lindex $tail 0] + ($dx > 0 ? 1 : -1)}]
            }
            if {$dy != 0} {
                lset tail 1 [expr {[lindex $tail 1] + ($dy > 0 ? 1 : -1)}]
            }
        }
        
        # Mark visited
        dict set visited [list [lindex $tail 0] [lindex $tail 1]] 1
    }
}

# Print result
puts [dict size $visited]
