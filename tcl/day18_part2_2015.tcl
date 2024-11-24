
#!/usr/bin/env tclsh

set gridSize 100
set steps 100

proc countOnNeighbors {grid x y} {
    set on 0
    for {set dx -1} {$dx <= 1} {incr dx} {
        for {set dy -1} {$dy <= 1} {incr dy} {
            if {$dx == 0 && $dy == 0} continue
            
            set nx [expr {$x + $dx}]
            set ny [expr {$y + $dy}]
            
            if {$nx >= 0 && $nx < $::gridSize && $ny >= 0 && $ny < $::gridSize} {
                if {[lindex $grid $nx $ny]} {
                    incr on
                }
            }
        }
    }
    return $on
}

proc step {grid} {
    set newGrid [lrepeat $::gridSize [lrepeat $::gridSize 0]]
    
    for {set x 0} {$x < $::gridSize} {incr x} {
        for {set y 0} {$y < $::gridSize} {incr y} {
            set onNeighbors [countOnNeighbors $grid $x $y]
            
            if {[lindex $grid $x $y]} {
                set newGrid [lreplace $newGrid $x $x [lreplace [lindex $newGrid $x] $y $y [expr {$onNeighbors == 2 || $onNeighbors == 3}]]]
            } else {
                set newGrid [lreplace $newGrid $x $x [lreplace [lindex $newGrid $x] $y $y [expr {$onNeighbors == 3}]]]
            }
        }
    }
    
    # Ensure corners are always on
    set newGrid [lreplace $newGrid 0 0 [lreplace [lindex $newGrid 0] 0 0 1]]
    set newGrid [lreplace $newGrid 0 0 [lreplace [lindex $newGrid 0] [expr {$::gridSize - 1}] [expr {$::gridSize - 1}] 1]]
    set newGrid [lreplace $newGrid [expr {$::gridSize - 1}] [expr {$::gridSize - 1}] [lreplace [lindex $newGrid [expr {$::gridSize - 1}]] 0 0 1]]
    set newGrid [lreplace $newGrid [expr {$::gridSize - 1}] [expr {$::gridSize - 1}] [lreplace [lindex $newGrid [expr {$::gridSize - 1}]] [expr {$::gridSize - 1}] [expr {$::gridSize - 1}] 1]]
    
    return $newGrid
}

# Read input file
set file [open "input.txt" r]
set content [read $file]
close $file

# Initialize grid
set grid [lrepeat $gridSize [lrepeat $gridSize 0]]

# Parse input
set lines [split $content "\n"]
for {set y 0} {$y < [llength $lines]} {incr y} {
    set line [lindex $lines $y]
    for {set x 0} {$x < [string length $line]} {incr x} {
        if {[string index $line $x] eq "#"} {
            set grid [lreplace $grid $x $x [lreplace [lindex $grid $x] $y $y 1]]
        }
    }
}

# Set corners always on
set grid [lreplace $grid 0 0 [lreplace [lindex $grid 0] 0 0 1]]
set grid [lreplace $grid 0 0 [lreplace [lindex $grid 0] [expr {$gridSize - 1}] [expr {$gridSize - 1}] 1]]
set grid [lreplace $grid [expr {$gridSize - 1}] [expr {$gridSize - 1}] [lreplace [lindex $grid [expr {$gridSize - 1}]] 0 0 1]]
set grid [lreplace $grid [expr {$gridSize - 1}] [expr {$gridSize - 1}] [lreplace [lindex $grid [expr {$gridSize - 1}]] [expr {$gridSize - 1}] [expr {$gridSize - 1}] 1]]

# Run simulation
for {set i 0} {$i < $steps} {incr i} {
    set grid [step $grid]
}

# Count on lights
set onCount 0
foreach row $grid {
    foreach light $row {
        if {$light} {
            incr onCount
        }
    }
}

puts $onCount
