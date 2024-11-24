
# Read the input from input.txt
set fileId [open "input.txt" r]
set steps [gets $fileId]
close $fileId

# Initialize the circular buffer with the initial value 0
set buffer [list 0]
set currentPosition 0

# Perform the spinlock operations 2017 times
for {set i 1} {$i <= 2017} {incr i} {
    # Calculate the new position to insert the value
    set newPosition [expr {($currentPosition + $steps) % [llength $buffer]}]
    
    # Insert the new value at the calculated position
    set buffer [linsert $buffer [expr {$newPosition + 1}] $i]
    
    # Update the current position to the new value's position
    set currentPosition [expr {$newPosition + 1}]
}

# Find the value after 2017 in the circular buffer
set index [lsearch -exact $buffer 2017]
set result [lindex $buffer [expr {($index + 1) % [llength $buffer]}]]

# Print the result
puts $result
