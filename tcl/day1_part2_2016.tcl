
# Function to calculate Manhattan distance
proc manhattan_distance {x y} {
  return [expr abs($x) + abs($y)]
}

# Function to solve Part 1
proc solve_part1 {instructions} {
  set x 0
  set y 0
  set direction 0 ;# 0: North, 1: East, 2: South, 3: West

  foreach instruction $instructions {
    regexp {([LR])(\d+)} $instruction - turn distance
    
    if {$turn eq "R"} {
      incr direction
    } else {
      incr direction -1
    }
    set direction [expr {($direction + 4) % 4}] ;# Normalize direction

    switch $direction {
      0 { incr y $distance } ;# North
      1 { incr x $distance } ;# East
      2 { incr y -$distance } ;# South
      3 { incr x -$distance } ;# West
    }
  }
  
  return [manhattan_distance $x $y]
}

# Function to solve Part 2
proc solve_part2 {instructions} {
  set x 0
  set y 0
  set direction 0 ;# 0: North, 1: East, 2: South, 3: West
  set visited {}

  lappend visited "0,0"

  foreach instruction $instructions {
    regexp {([LR])(\d+)} $instruction - turn distance
    
    if {$turn eq "R"} {
      incr direction
    } else {
      incr direction -1
    }
    set direction [expr {($direction + 4) % 4}] ;# Normalize direction

    for {set i 0} {$i < $distance} {incr i} {
      switch $direction {
        0 { incr y 1 } ;# North
        1 { incr x 1 } ;# East
        2 { incr y -1 } ;# South
        3 { incr x -1 } ;# West
      }

      set location "$x,$y"
      if {[lsearch -exact $visited $location] != -1} {
        return [manhattan_distance $x $y]
      }
      lappend visited $location
    }
  }
  return "No location visited twice"
}

# Main entry point
proc main {} {
  if {[file exists "input.txt"]} {
    set file [open "input.txt" r]
    set content [read $file]
    close $file
    
    # Split the content by comma and trim whitespace
    set instructions [split $content ","]
    set trimmedInstructions {}
    foreach instruction $instructions {
      lappend trimmedInstructions [string trim $instruction]
    }

    puts "Part 1: [solve_part1 $trimmedInstructions]"
    puts "Part 2: [solve_part2 $trimmedInstructions]"
  } else {
    puts "Error: input.txt not found."
  }
}

# Call the main function
main
