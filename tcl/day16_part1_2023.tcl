
#!/usr/bin/env tclsh

# Constants
set EMPTY "."
set ASCENDING_MIRROR "/"
set DESCENDING_MIRROR "\\"
set VERTICAL_SPLITTER "|"
set HORIZONTAL_SPLITTER "-"

# Directions
array set DIRS {
    NORTH {0 -1}
    WEST  {-1 0}
    SOUTH {0 1}
    EAST  {1 0}
}

# Read input file
proc read_input {filename} {
    set fp [open $filename r]
    set data [read $fp]
    close $fp
    return [split [string trim $data] "\n"]
}

# Build grid from input
proc build_grid {input} {
    upvar grid grid
    set grid(width) [string length [lindex $input 0]]
    set grid(height) [llength $input]

    for {set y 0} {$y < $grid(height)} {incr y} {
        set line [lindex $input $y]
        for {set x 0} {$x < $grid(width)} {incr x} {
            set char [string index $line $x]
            if {$char ne "."} {
                set grid($x,$y) $char
            }
        }
    }
}

# Check if coordinate is in bounds
proc is_in_bounds {x y} {
    upvar grid grid
    return [expr {$x >= 0 && $x < $grid(width) && $y >= 0 && $y < $grid(height)}]
}

# Rotate coordinates
proc rotate_90 {dx dy} {
    return [list $dy [expr {-$dx}]]
}

proc rotate_neg_90 {dx dy} {
    return [list [expr {-$dy}] $dx]
}

# Calculate next beam(s)
proc next_beam {origin_x origin_y dx dy} {
    upvar grid grid
    set beams {}

    if {![info exists grid($origin_x,$origin_y)]} {
        return [list [list [expr {$origin_x + $dx}] [expr {$origin_y + $dy}] $dx $dy]]
    }

    set char $grid($origin_x,$origin_y)
    switch -exact $char {
        "/" {
            if {$dx == 0} {
                lassign [rotate_neg_90 $dx $dy] new_dx new_dy
            } else {
                lassign [rotate_90 $dx $dy] new_dx new_dy
            }
            lappend beams [list [expr {$origin_x + $new_dx}] [expr {$origin_y + $new_dy}] $new_dx $new_dy]
        }
        "\\" {
            if {$dx == 0} {
                lassign [rotate_90 $dx $dy] new_dx new_dy
            } else {
                lassign [rotate_neg_90 $dx $dy] new_dx new_dy
            }
            lappend beams [list [expr {$origin_x + $new_dx}] [expr {$origin_y + $new_dy}] $new_dx $new_dy]
        }
        "|" {
            if {$dx != 0} {
                lassign [rotate_90 $dx $dy] new_dx1 new_dy1
                lassign [rotate_neg_90 $dx $dy] new_dx2 new_dy2
                lappend beams [list [expr {$origin_x + $new_dx1}] [expr {$origin_y + $new_dy1}] $new_dx1 $new_dy1]
                lappend beams [list [expr {$origin_x + $new_dx2}] [expr {$origin_y + $new_dy2}] $new_dx2 $new_dy2]
            } else {
                lappend beams [list [expr {$origin_x + $dx}] [expr {$origin_y + $dy}] $dx $dy]
            }
        }
        "-" {
            if {$dy != 0} {
                lassign [rotate_90 $dx $dy] new_dx1 new_dy1
                lassign [rotate_neg_90 $dx $dy] new_dx2 new_dy2
                lappend beams [list [expr {$origin_x + $new_dx1}] [expr {$origin_y + $new_dy1}] $new_dx1 $new_dy1]
                lappend beams [list [expr {$origin_x + $new_dx2}] [expr {$origin_y + $new_dy2}] $new_dx2 $new_dy2]
            } else {
                lappend beams [list [expr {$origin_x + $dx}] [expr {$origin_y + $dy}] $dx $dy]
            }
        }
        default {
            lappend beams [list [expr {$origin_x + $dx}] [expr {$origin_y + $dy}] $dx $dy]
        }
    }

    return $beams
}

# Calculate beam propagation
proc calculate_propagation {start_x start_y start_dx start_dy} {
    upvar grid grid
    array set seen {}
    set to_explore [list [list $start_x $start_y $start_dx $start_dy]]

    while {[llength $to_explore] > 0} {
        lassign [lindex $to_explore 0] origin_x origin_y dx dy
        set to_explore [lrange $to_explore 1 end]

        set beam_key "$origin_x,$origin_y,$dx,$dy"
        if {[is_in_bounds $origin_x $origin_y] && ![info exists seen($beam_key)]} {
            set seen($beam_key) 1
            foreach new_beam [next_beam $origin_x $origin_y $dx $dy] {
                lappend to_explore $new_beam
            }
        }
    }

    return [array names seen]
}

# Calculate energized tiles
proc calculate_energization {propagation} {
    array set energized {}
    foreach beam $propagation {
        lassign [split $beam ","] x y
        set energized($x,$y) 1
    }
    return [array names energized]
}

# Solve the problem
proc solve {input} {
    build_grid $input
    set start [list 0 0 1 0]
    lassign $start start_x start_y start_dx start_dy

    set propagation [calculate_propagation $start_x $start_y $start_dx $start_dy]
    set energization [calculate_energization $propagation]

    return [llength $energization]
}

# Main
set input [read_input "input.txt"]
puts [solve $input]
