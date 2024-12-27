
#!/usr/bin/env tclsh

# Function to read data from file and return as lists
proc read_data {filename} {
    set left_list {}
    set right_list {}
    if {[catch {open $filename r} file]} {
        puts "Error opening file: $filename"
        return
    }
    while {[gets $file line] >= 0} {
        if {[regexp {^(\d+)\s+(\d+)$} $line match left right]} {
            lappend left_list $left
            lappend right_list $right
        }
    }
    close $file
    return [list $left_list $right_list]
}

# Function to calculate total distance
proc calculate_distance {left_list right_list} {
    set sorted_left [lsort -integer $left_list]
    set sorted_right [lsort -integer $right_list]
    set total_distance 0
    for {set i 0} {$i < [llength $sorted_left]} {incr i} {
        set distance [expr abs([lindex $sorted_left $i] - [lindex $sorted_right $i])]
        set total_distance [expr $total_distance + $distance]
    }
    return $total_distance
}

# Function to calculate similarity score
proc calculate_similarity {left_list right_list} {
    set total_similarity 0
    foreach left_num $left_list {
        set count 0
        foreach right_num $right_list {
            if {$left_num == $right_num} {
                incr count
            }
        }
        set total_similarity [expr $total_similarity + ($left_num * $count)]
    }
    return $total_similarity
}

# Main program
set filename "input.txt"
if {[catch {read_data $filename} data]} {
    puts "Error reading data: $data"
    exit 1
}
lassign $data left_list right_list

# Part 1: Calculate total distance
set total_distance [calculate_distance $left_list $right_list]
puts "Total distance: $total_distance"

# Part 2: Calculate similarity score
set similarity_score [calculate_similarity $left_list $right_list]
puts "Similarity score: $similarity_score"
