
# Function to parse the input file and build a distance matrix
proc parseInput {filename} {
    set distances {}
    set locations {}

    set fp [open $filename r]
    while {[gets $fp line] >= 0} {
        if {[regexp {(\w+) to (\w+) = (\d+)} $line -> from to distance]} {
            if {[lsearch -exact $locations $from] == -1} {
                lappend locations $from
            }
            if {[lsearch -exact $locations $to] == -1} {
                lappend locations $to
            }
            dict set distances $from $to $distance
            dict set distances $to $from $distance
        }
    }
    close $fp

    return [list $distances $locations]
}

# Function to calculate the total distance of a route
proc calculateDistance {distances route} {
    set totalDistance 0
    for {set i 0} {$i < [expr {[llength $route] - 1}]} {incr i} {
        set from [lindex $route $i]
        set to [lindex $route [expr {$i + 1}]]
        set totalDistance [expr {$totalDistance + [dict get $distances $from $to]}]
    }
    return $totalDistance
}

# Function to generate all permutations of a list
proc permute {list} {
    if {[llength $list] <= 1} {
        return [list $list]
    }
    set result {}
    foreach item $list {
        set sublist [lsearch -all -inline -not -exact $list $item]
        foreach perm [permute $sublist] {
            lappend result [linsert $perm 0 $item]
        }
    }
    return $result
}

# Main function to find the shortest and longest routes
proc findRoutes {filename} {
    lassign [parseInput $filename] distances locations

    set allRoutes [permute $locations]
    set shortestDistance [expr {2 ** 31 - 1}]
    set longestDistance 0

    foreach route $allRoutes {
        set distance [calculateDistance $distances $route]
        if {$distance < $shortestDistance} {
            set shortestDistance $distance
        }
        if {$distance > $longestDistance} {
            set longestDistance $distance
        }
    }

    puts "Shortest route distance: $shortestDistance"
    puts "Longest route distance: $longestDistance"
}

# Call the main function with the input file
findRoutes "input.txt"
