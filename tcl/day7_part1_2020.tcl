
#!/usr/bin/tclsh

proc parse_rules {filename} {
    set rules {}
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        # Split the line into the container bag and its contents
        regexp {^([a-z\s]+) bags contain (.*)\.$} $line _ container contents

        # Parse the contents of the bag
        set contents_list [split $contents ,]
        set bag_contents {}
        foreach content $contents_list {
            if {[string match "no other bags" $content]} {
                # Bag contains no other bags
                continue
            } else {
                # Extract the quantity and bag color
                regexp {^\s*([0-9]+) ([a-z\s]+) bags?$} $content _ quantity color
                dict set bag_contents $color $quantity
            }
        }
        dict set rules $container $bag_contents
    }
    close $file
    return $rules
}

proc can_contain {rules bag_color target_color} {
    # Base case: If the bag directly contains the target color, return 1
    if {[dict exists $rules $bag_color]} {
        if {[dict exists [dict get $rules $bag_color] $target_color]} {
            return 1
        }

        # Recursive case: Check if any of the bags contained within can contain the target color
        foreach contained_bag [dict keys [dict get $rules $bag_color]] {
            if {[can_contain $rules $contained_bag $target_color]} {
                return 1
            }
        }
    }
    
    # If no conditions are met, return 0
    return 0
}

proc solve {filename target_color} {
    set rules [parse_rules $filename]
    set count 0
    foreach bag_color [dict keys $rules] {
        if {[can_contain $rules $bag_color $target_color]} {
            incr count
        }
    }
    return $count
}

# Main execution
set filename "input.txt"
set target_color "shiny gold"
set result [solve $filename $target_color]
puts $result
