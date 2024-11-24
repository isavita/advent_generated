
#!/usr/bin/env tclsh

# Directions dictionary
array set directions {
    e   {1 0}
    se  {0 1}
    sw  {-1 1}
    w   {-1 0}
    nw  {0 -1}
    ne  {1 -1}
}

# Get neighbors of a tile
proc get_neighbors {tile} {
    global directions
    set neighbors {}
    foreach {_ dir} [array get directions] {
        lassign $tile q r
        lassign $dir dq dr
        lappend neighbors [list [expr {$q + $dq}] [expr {$r + $dr}]]
    }
    return $neighbors
}

# Read input file
set fp [open "input.txt" r]
set file_data [read $fp]
close $fp

# Process input lines
array set black_tiles {}
foreach line [split $file_data "\n"] {
    if {$line eq ""} continue
    set coord {0 0}
    set i 0
    while {$i < [string length $line]} {
        set dir [string range $line $i [expr {$i + 1}]]
        if {$dir ni {ne nw se sw}} {
            set dir [string index $line $i]
            set i [expr {$i + 1}]
        } else {
            set i [expr {$i + 2}]
        }
        
        lassign $directions($dir) dq dr
        lassign $coord q r
        set coord [list [expr {$q + $dq}] [expr {$r + $dr}]]
    }
    
    if {[info exists black_tiles($coord)]} {
        unset black_tiles($coord)
    } else {
        set black_tiles($coord) 1
    }
}

# Simulate 100 days
for {set day 0} {$day < 100} {incr day} {
    array unset tiles_to_check
    array set tiles_to_check {}
    
    foreach tile [array names black_tiles] {
        set tiles_to_check($tile) 1
        foreach neighbor [get_neighbors $tile] {
            set tiles_to_check($neighbor) 1
        }
    }
    
    array unset new_black_tiles
    array set new_black_tiles {}
    
    foreach tile [array names tiles_to_check] {
        set black_neighbor_count 0
        foreach neighbor [get_neighbors $tile] {
            if {[info exists black_tiles($neighbor)]} {
                incr black_neighbor_count
            }
        }
        
        if {[info exists black_tiles($tile)]} {
            if {$black_neighbor_count == 1 || $black_neighbor_count == 2} {
                set new_black_tiles($tile) 1
            }
        } else {
            if {$black_neighbor_count == 2} {
                set new_black_tiles($tile) 1
            }
        }
    }
    
    array unset black_tiles
    array set black_tiles [array get new_black_tiles]
}

# Print result
puts [array size black_tiles]
