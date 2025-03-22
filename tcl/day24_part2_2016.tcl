
package require struct::list

# Helper function to read the map from file
proc read_map {filename} {
    set map {}
    set y 0
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        set map [lappend map $line]
        incr y
    }
    close $file
    return $map
}

# Helper function to find the coordinates of a specific location
proc find_location {map target} {
    for {set y 0} {$y < [llength $map]} {incr y} {
        set row [lindex $map $y]
        for {set x 0} {$x < [string length $row]} {incr x} {
            if {[string index $row $x] eq $target} {
                return [list $x $y]
            }
        }
    }
    return {}
}

# Helper function to perform breadth-first search (BFS) to find the shortest path between two locations
proc bfs {map start end} {
    set width [string length [lindex $map 0]]
    set height [llength $map]
    set visited [dict create]
    set queue [list [list $start 0]] ;# {location steps}
    dict set visited $start 1

    while {[llength $queue] > 0} {
        set current [lindex $queue 0]
        set queue [lrange $queue 1 end]
        set location [lindex $current 0]
        set steps [lindex $current 1]
        lassign $location x y

        if {$location eq $end} {
            return $steps
        }

        # Possible moves: up, down, left, right
        set moves [list \
                       [list [expr {$x}] [expr {$y - 1}]] \
                       [list [expr {$x}] [expr {$y + 1}]] \
                       [list [expr {$x - 1}] [expr {$y}]] \
                       [list [expr {$x + 1}] [expr {$y}]] \
                  ]

        foreach {nxny} $moves {
            lassign $nxny nx ny
            if {$nx < 0 || $nx >= $width || $ny < 0 || $ny >= $height} continue
            set next_char [string index [lindex $map $ny] $nx]
            if {$next_char eq "#"} continue

            set next_location [list $nx $ny]
            if {![dict exists $visited $next_location]} {
                dict set visited $next_location 1
                set queue [lappend queue [list $next_location [expr {$steps + 1}]]]
            }
        }
    }

    return Inf ;# unreachable
}

# Main function to solve the problem
proc solve {filename {part 1}} {
    set map [read_map $filename]
    set locations {}
    for {set i 0} {$i <= 9} {incr i} {
        set loc [find_location $map $i]
        if {$loc ne {}} {
            dict set locations $i $loc
        }
    }

    set distances [dict create]
    foreach {from_num from_loc} [dict get $locations] {
        foreach {to_num to_loc} [dict get $locations] {
            if {$from_num eq $to_num} continue
            set distance [bfs $map $from_loc $to_loc]
            dict set distances $from_num,$to_num $distance
        }
    }

    set other_locations [list]
    foreach num [dict keys $locations] {
        if {$num != 0} {
            lappend other_locations $num
        }
    }

    set min_distance Inf
    foreach permutation [struct::list permutations $other_locations] {
        set current_distance 0
        set current_location 0
        foreach next_location $permutation {
            set key "$current_location,$next_location"
            set distance [dict get $distances $key]
            incr current_distance $distance
            set current_location $next_location
        }
        if {$part eq 2} {
            set key "$current_location,0"
            set distance [dict get $distances $key]
            incr current_distance $distance
        }

        if {$current_distance < $min_distance} {
            set min_distance $current_distance
        }
    }

    return $min_distance
}

# Main entry point
if {[info exists argv0] && [file tail $argv0] eq [file tail [info script]]} {
    # Part 1
    set result_part1 [solve "input.txt" 1]
    puts "Part 1: $result_part1"

    # Part 2
    set result_part2 [solve "input.txt" 2]
    puts "Part 2: $result_part2"
}
