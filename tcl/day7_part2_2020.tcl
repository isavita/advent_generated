
# Function to parse the input file and create a bag rules dictionary
proc parse_input {filename} {
  set rules {}
  set file [open $filename r]
  while {[gets $file line] != -1} {
    # Split the line into the container bag and its contents
    regexp {^([a-z\s]+) bags contain (.*)\.$} $line _ container contents

    # Remove " bags" from container
    regsub { bags$} $container "" container

    set contained_bags {}
    # If the contents are "no other bags", then the bag is empty
    if {$contents eq "no other bags"} {
      set contained_bags {}
    } else {
      # Split the contents into individual bag rules
      foreach bag_rule [split $contents ,] {
        # Extract the quantity and color of the contained bag
        regexp {^\s*([0-9]+) ([a-z\s]+) bags?$} $bag_rule _ quantity color
        
        # Add the contained bag and its quantity to the contained bags dictionary
        dict set contained_bags $color $quantity
      }
    }
    # Add the rule to the rules dictionary
    dict set rules $container $contained_bags
  }
  close $file
  return $rules
}

# Function to find all bags that can eventually contain a shiny gold bag
proc find_containers {rules target_bag} {
  set containers {}
  foreach {container contents} [dict get $rules] {
    if {[dict exists $contents $target_bag]} {
      lappend containers $container
      lappend containers [find_containers $rules $container]
    }
  }
  return [lsort -unique $containers]
}

# Function to count the number of bags required inside a given bag
proc count_bags {rules bag_color} {
  set total_bags 0
  set contained_bags [dict get $rules $bag_color]
  foreach {color quantity} [dict get $rules $bag_color] {
    set total_bags [expr {$total_bags + $quantity + $quantity * [count_bags $rules $color]}]
  }
  return $total_bags
}

# Main entry point
proc main {} {
  # Parse the input file
  set rules [parse_input "input.txt"]

  # Part 1: Find the number of bags that can contain a shiny gold bag
  set shiny_gold_containers [find_containers $rules "shiny gold"]
  puts "Part 1: [llength $shiny_gold_containers]"

  # Part 2: Count the number of bags required inside a shiny gold bag
  set bags_inside_shiny_gold [count_bags $rules "shiny gold"]
  puts "Part 2: $bags_inside_shiny_gold"
}

# Execute the main function
main
