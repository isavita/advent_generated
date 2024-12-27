
# Initialize a 1000x1000 grid of lights, all initially off (brightness 0)
set grid [lrepeat 1000 [lrepeat 1000 0]]

# Open the input file
set file [open "input.txt" r]

# Process each instruction
while {[gets $file line] != -1} {
    # Parse the instruction using regular expressions
    regexp {^(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)$} $line -> action x1 y1 x2 y2

    # Iterate over the specified rectangle of lights
    for {set i $y1} {$i <= $y2} {incr i} {
        for {set j $x1} {$j <= $x2} {incr j} {
            # Apply the action to each light, updating brightness
            set current_brightness [lindex [lindex $grid $i] $j]
            switch -- $action {
                "turn on"  { set new_brightness [expr {$current_brightness + 1}] }
                "turn off" { set new_brightness [expr {max(0, $current_brightness - 1)}] }
                "toggle"   { set new_brightness [expr {$current_brightness + 2}] }
            }
            lset grid $i $j $new_brightness
        }
    }
}

# Close the input file
close $file

# Calculate the total brightness
set total_brightness 0
foreach row $grid {
    foreach brightness $row {
        incr total_brightness $brightness
    }
}

# Print the total brightness
puts "Total brightness: $total_brightness"
