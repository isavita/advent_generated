
#!/usr/bin/env tclsh

# Read input file
set input [open "input.txt" r]
set cubes [dict create]
set min_x [expr {2**31 - 1}]
set min_y [expr {2**31 - 1}]
set min_z [expr {2**31 - 1}]
set max_x [expr {-2**31}]
set max_y [expr {-2**31}]
set max_z [expr {-2**31}]

# Parse input and track cubes
while {[gets $input line] >= 0} {
    if {$line eq ""} continue
    
    lassign [split $line ","] x y z
    dict set cubes "$x,$y,$z" 1
    
    set min_x [expr {min($min_x, $x)}]
    set min_y [expr {min($min_y, $y)}]
    set min_z [expr {min($min_z, $z)}]
    set max_x [expr {max($max_x, $x)}]
    set max_y [expr {max($max_y, $y)}]
    set max_z [expr {max($max_z, $z)}]
}
close $input

# Adjust boundaries
incr min_x -1
incr min_y -1
incr min_z -1
incr max_x 1
incr max_y 1
incr max_z 1

# Neighbors
set neighbors {
    {-1 0 0}
    {1 0 0}
    {0 -1 0}
    {0 1 0}
    {0 0 -1}
    {0 0 1}
}

# BFS to count external faces
set faces 0
set queue [list "$min_x,$min_y,$min_z"]
set seen [dict create "$min_x,$min_y,$min_z" 1]

while {[llength $queue] > 0} {
    set curr [lindex $queue 0]
    set queue [lrange $queue 1 end]
    
    foreach neighbor $neighbors {
        lassign $neighbor dx dy dz
        lassign [split $curr ","] cx cy cz
        
        set nx [expr {$cx + $dx}]
        set ny [expr {$cy + $dy}]
        set nz [expr {$cz + $dz}]
        
        set next "$nx,$ny,$nz"
        
        # Check boundaries
        if {$nx < $min_x || $nx > $max_x ||
            $ny < $min_y || $ny > $max_y ||
            $nz < $min_z || $nz > $max_z} continue
        
        # Check if cube exists
        if {[dict exists $cubes $next]} {
            incr faces
        } elseif {![dict exists $seen $next]} {
            dict set seen $next 1
            lappend queue $next
        }
    }
}

puts $faces
