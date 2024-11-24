
#!/usr/bin/env tclsh

# Read input file
set input [split [string trim [read [open "input.txt" r]]] "\n"]

# Create list of numbers with their original positions
set nums {}
for {set i 0} {$i < [llength $input]} {incr i} {
    lappend nums [list $i [lindex $input $i]]
}

# Mix the numbers
proc mix {numsVar} {
    upvar $numsVar nums
    set n [expr {[llength $nums] - 1}]
    
    for {set i 0} {$i < [llength $nums]} {incr i} {
        set oldpos [lindex [lindex $nums $i] 0]
        set val [lindex [lindex $nums $i] 1]
        
        set newpos [expr {(($oldpos + $val) % $n + $n) % $n}]
        
        # Adjust positions
        set tempNums {}
        foreach item $nums {
            set pos [lindex $item 0]
            if {$oldpos < $newpos} {
                if {$pos > $oldpos && $pos <= $newpos} {
                    lappend tempNums [list [expr {$pos - 1}] [lindex $item 1]]
                } else {
                    lappend tempNums $item
                }
            } elseif {$newpos < $oldpos} {
                if {$pos >= $newpos && $pos < $oldpos} {
                    lappend tempNums [list [expr {$pos + 1}] [lindex $item 1]]
                } else {
                    lappend tempNums $item
                }
            } else {
                lappend tempNums $item
            }
        }
        
        set nums $tempNums
        
        # Update the current item's position
        set nums [lreplace $nums $i $i [list $newpos [lindex [lindex $nums $i] 1]]]
    }
}

# Find coordinates
proc coords {nums} {
    set l [llength $nums]
    
    # Find zero position
    set zeroPos -1
    for {set i 0} {$i < $l} {incr i} {
        if {[lindex [lindex $nums $i] 1] == 0} {
            set zeroPos [lindex [lindex $nums $i] 0]
            break
        }
    }
    
    set sum 0
    foreach i {1000 2000 3000} {
        set checkPos [expr {($zeroPos + $i) % $l}]
        foreach item $nums {
            if {[lindex $item 0] == $checkPos} {
                set sum [expr {$sum + [lindex $item 1]}]
                break
            }
        }
    }
    
    return $sum
}

# Mix the numbers
mix nums

# Print the result
puts [coords $nums]
