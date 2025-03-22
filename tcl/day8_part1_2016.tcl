
#!/usr/bin/env tclsh

# Define screen dimensions
set SCREEN_WIDTH 50
set SCREEN_HEIGHT 6

# Initialize screen (all pixels off)
proc init_screen {} {
    global SCREEN_WIDTH SCREEN_HEIGHT screen
    for {set y 0} {$y < $SCREEN_HEIGHT} {incr y} {
        for {set x 0} {$x < $SCREEN_WIDTH} {incr x} {
            set screen($x,$y) 0
        }
    }
}

# Function to execute a rectangle command
proc rect {a b} {
    global screen SCREEN_WIDTH SCREEN_HEIGHT
    for {set x 0} {$x < $a} {incr x} {
        for {set y 0} {$y < $b} {incr y} {
            set screen($x,$y) 1
        }
    }
}

# Function to execute a rotate row command
proc rotate_row {y b} {
    global screen SCREEN_WIDTH
    set temp_row {}
    for {set x 0} {$x < $SCREEN_WIDTH} {incr x} {
        lappend temp_row $screen($x,$y)
    }
    for {set x 0} {$x < $SCREEN_WIDTH} {incr x} {
        set new_x [expr {($x + $b) % $SCREEN_WIDTH}]
        set screen($new_x,$y) [lindex $temp_row $x]
    }
}

# Function to execute a rotate column command
proc rotate_column {x b} {
    global screen SCREEN_HEIGHT
    set temp_col {}
    for {set y 0} {$y < $SCREEN_HEIGHT} {incr y} {
        lappend temp_col $screen($x,$y)
    }
    for {set y 0} {$y < $SCREEN_HEIGHT} {incr y} {
        set new_y [expr {($y + $b) % $SCREEN_HEIGHT}]
        set screen($x,$new_y) [lindex $temp_col $y]
    }
}

# Function to count the number of lit pixels
proc count_lit_pixels {} {
    global screen SCREEN_WIDTH SCREEN_HEIGHT
    set count 0
    for {set y 0} {$y < $SCREEN_HEIGHT} {incr y} {
        for {set x 0} {$x < $SCREEN_WIDTH} {incr x} {
            if {$screen($x,$y) == 1} {
                incr count
            }
        }
    }
    return $count
}

# Function to print the screen (for debugging)
proc print_screen {} {
    global screen SCREEN_WIDTH SCREEN_HEIGHT
    for {set y 0} {$y < $SCREEN_HEIGHT} {incr y} {
        for {set x 0} {$x < $SCREEN_WIDTH} {incr x} {
            if {$screen($x,$y) == 1} {
                puts -nonewline "#"
            } else {
                puts -nonewline "."
            }
        }
        puts ""
    }
}

# Main program
proc main {} {
    init_screen
    
    # Read instructions from input file
    if {[catch {open "input.txt" r} file_id]} {
        puts "Error opening input file: $file_id"
        return 1
    }
    
    while {[gets $file_id line] >= 0} {
        if {[regexp {rect (\d+)x(\d+)} $line dummy a b]} {
            rect $a $b
        } elseif {[regexp {rotate row y=(\d+) by (\d+)} $line dummy y b]} {
            rotate_row $y $b
        } elseif {[regexp {rotate column x=(\d+) by (\d+)} $line dummy x b]} {
            rotate_column $x $b
        }
    }
    close $file_id

    # Count and print the number of lit pixels
    set lit_pixels [count_lit_pixels]
    puts "Number of lit pixels: $lit_pixels"

    # Optionally, print the final screen
    # print_screen 
}

# Run the main program
main
