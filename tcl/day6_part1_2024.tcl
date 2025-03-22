
# Main entry point
proc main {} {
  # Read the input from file
  set filename "input.txt"
  if {[catch {set file [open $filename r]} err]} {
    puts "Error opening file: $filename - $err"
    return 1
  }
  set map [read $file]
  close $file

  # Parse the map into a 2D array
  set lines [split $map "\n"]
  set height [llength $lines]
  set width [string length [lindex $lines 0]]
  set grid {}

  for {set i 0} {$i < $height} {incr i} {
    lappend grid [split [lindex $lines $i] ""]
  }

  # Find the starting position of the guard
  set x -1
  set y -1
  set direction {}
  for {set i 0} {$i < $height} {incr i} {
    for {set j 0} {$j < $width} {incr j} {
      set cell [lindex [lindex $grid $i] $j]
      if {$cell eq "^"} {
        set x $j
        set y $i
        set direction "up"
        break
      } elseif {$cell eq ">"} {
        set x $j
        set y $i
        set direction "right"
        break
      } elseif {$cell eq "v"} {
        set x $j
        set y $i
        set direction "down"
        break
      } elseif {$cell eq "<"} {
        set x $j
        set y $i
        set direction "left"
        break
      }
    }
    if {$x != -1} {
      break
    }
  }

  # Initialize the visited positions set
  set visited {}
  lappend visited "$x,$y"

  # Simulate the guard's movement
  while {1} {
    # Determine the next position
    switch $direction {
      "up" {
        set next_x $x
        set next_y [expr {$y - 1}]
      }
      "down" {
        set next_x $x
        set next_y [expr {$y + 1}]
      }
      "left" {
        set next_x [expr {$x - 1}]
        set next_y $y
      }
      "right" {
        set next_x [expr {$x + 1}]
        set next_y $y
      }
    }

    # Check if the next position is valid
    if {$next_x < 0 || $next_x >= $width || $next_y < 0 || $next_y >= $height} {
      break
    }

    # Check if there is an obstacle in front of the guard
    set next_cell [lindex [lindex $grid $next_y] $next_x]
    if {$next_cell eq "#"} {
      # Turn right
      switch $direction {
        "up" {
          set direction "right"
        }
        "down" {
          set direction "left"
        }
        "left" {
          set direction "up"
        }
        "right" {
          set direction "down"
        }
      }
    } else {
      # Move forward
      set x $next_x
      set y $next_y

      # Add the new position to the visited set
      set pos "$x,$y"
      if {[lsearch -exact $visited $pos] == -1} {
        lappend visited $pos
      }
    }
  }

  # Print the number of distinct positions visited
  puts [llength $visited]
}

# Call the main function
main
