
proc loops {grid start_x start_y start_dir} {
    set h [llength $grid]
    set w [llength [lindex $grid 0]]
    set dirs {{0 -1} {1 0} {0 1} {-1 0}}  ;# up, right, down, left
    
    set x $start_x
    set y $start_y
    set dir_idx $start_dir
    set seen {}
    
    # Use fixed number of steps as loop detection
    for {set i 0} {$i < 2000000} {incr i} {
        set state "$x,$y,$dir_idx"
        if {[dict exists $seen $state]} {
            return 1
        }
        dict set seen $state 1
        
        lassign [lindex $dirs $dir_idx] dir_x dir_y
        set nx [expr {$x + $dir_x}]
        set ny [expr {$y + $dir_y}]
        
        # Check if leaving grid
        if {$nx < 0 || $nx >= $w || $ny < 0 || $ny >= $h} {
            return 0
        }
            
        # If obstacle found, turn right
        if {[lindex [lindex $grid $ny] $nx] eq "#"} {
            set dir_idx [expr {($dir_idx + 1) % 4}]
            continue
        }
            
        # Move to new position
        set x $nx
        set y $ny
    }
    
    return 0
}

proc find_loop_positions {input_str} {
    # Create grid
    set grid {}
    foreach line [split $input_str \n] {
        set line [string trim $line]
        if {$line ne ""} {
            lappend grid [split $line ""]
        }
    }
    set h [llength $grid]
    set w [llength [lindex $grid 0]]
    
    # Find starting position and direction
    set start_x ""
    set start_y ""
    set start_dir 0  ;# Default to up
    for {set i 0} {$i < $h} {incr i} {
        for {set j 0} {$j < $w} {incr j} {
            set cell [lindex [lindex $grid $i] $j]
            if {$cell eq "^"} {
                set start_x $j
                set start_y $i
                set start_dir 0
            } elseif {$cell eq ">"} {
                set start_x $j
                set start_y $i
                set start_dir 1
            } elseif {$cell eq "v"} {
                set start_x $j
                set start_y $i
                set start_dir 2
            } elseif {$cell eq "<"} {
                set start_x $j
                set start_y $i
                set start_dir 3
            }
        }
    }
    
    # Convert starting position to empty space
    lset grid $start_y $start_x "."
    
    # Count positions that create loops
    set can_loop 0
    for {set y 0} {$y < $h} {incr y} {
        for {set x 0} {$x < $w} {incr x} {
            if {$x == $start_x && $y == $start_y} {
                continue
            }
            if {[lindex [lindex $grid $y] $x] ne "."} {
                continue
            }
                
            # Try placing obstacle
            lset grid $y $x "#"
            if {[loops $grid $start_x $start_y $start_dir]} {
                incr can_loop
            }
            lset grid $y $x "."
        }
    }
    
    return $can_loop
}

# Main entry point
set file [open "input.txt" r]
set input_str [read $file]
close $file

puts [find_loop_positions $input_str]
