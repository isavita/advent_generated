
#!/usr/bin/env tclsh

# Function to read numbers from a file into a list
proc read_numbers_from_file {filename} {
    set file [open $filename r]
    set left_list {}
    set right_list {}
    while {[gets $file line] != -1} {
        lassign $line left_num right_num
        lappend left_list $left_num
        lappend right_list $right_num
    }
    close $file
    return [list $left_list $right_list]
}

# Function to calculate the total distance between two lists
proc calculate_total_distance {left_list right_list} {
    set total_distance 0
    set sorted_left [lsort -integer $left_list]
    set sorted_right [lsort -integer $right_list]
    
    for {set i 0} {$i < [llength $sorted_left]} {incr i} {
        set left_num [lindex $sorted_left $i]
        set right_num [lindex $sorted_right $i]
        set distance [expr abs($left_num - $right_num)]
        incr total_distance $distance
    }
    return $total_distance
}

# Main program
if {[file exists "input.txt"]} {
    lassign [read_numbers_from_file "input.txt"] left_list right_list
    set total_distance [calculate_total_distance $left_list $right_list]
    puts $total_distance
} else {
    puts "Error: input.txt not found."
}
