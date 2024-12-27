
# Initialize a 1000x1000 grid of lights, all initially off (0).
set grid [lrepeat 1000 [lrepeat 1000 0]]

# Open the input file for reading.
set file [open "input.txt" r]

# Process each line (instruction) from the file.
while {[gets $file line] != -1} {
    # Extract command and coordinates using regular expression.
    regexp {^(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)$} $line -> cmd x1 y1 x2 y2

    # Iterate through the specified rectangle of lights.
    for {set i $y1} {$i <= $y2} {incr i} {
        for {set j $x1} {$j <= $x2} {incr j} {
            # Apply the command to each light.
            set light [lindex $grid $i $j]
            switch $cmd {
                "turn on"  { set light 1 }
                "turn off" { set light 0 }
                "toggle"   { set light [expr {!$light}] }
            }
            lset grid $i $j $light
        }
    }
}

# Close the input file.
close $file

# Count the number of lit lights.
set lit_count 0
foreach row $grid {
    foreach light $row {
        incr lit_count $light
    }
}

# Print the result to standard output.
puts $lit_count
