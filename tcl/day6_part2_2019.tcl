
#!/usr/bin/env tclsh

# Read input file
set fp [open "input.txt" r]
set file_data [read $fp]
close $fp

# Create data structures
array set nodes {}
array set parents {}

# Parse input and build orbit map
foreach line [split $file_data "\n"] {
    if {$line eq ""} continue
    lassign [split $line ")"] center orbiter
    
    if {![info exists nodes($center)]} {
        set nodes($center) 1
    }
    if {![info exists nodes($orbiter)]} {
        set nodes($orbiter) 1
    }
    
    set parents($orbiter) $center
}

# Create path to root function
proc path_to_root {node parents_arr} {
    upvar $parents_arr parents
    set path {}
    
    while {[info exists parents($node)]} {
        set node $parents($node)
        lappend path $node
    }
    
    return $path
}

# Find orbital transfers
set you_path [path_to_root "YOU" parents]
set san_path [path_to_root "SAN" parents]

# Find first common ancestor
set transfers 0
for {set i 0} {$i < [llength $you_path]} {incr i} {
    set you_node [lindex $you_path $i]
    set san_index [lsearch $san_path $you_node]
    
    if {$san_index != -1} {
        set transfers [expr {$i + $san_index}]
        break
    }
}

puts $transfers
