
proc coord_add {x1 y1 x2 y2} {
    return [list [expr {$x1 + $x2}] [expr {$y1 + $y2}]]
}

proc coord_multiply_by_scalar {x y s} {
    return [list [expr {$x * $s}] [expr {$y * $s}]]
}

proc abs_val {x} {
    if {$x < 0} {
        return [expr {-$x}]
    }
    return $x
}

proc parse_input {input_lines} {
    set north {0 -1}
    set west {-1 0}
    set south {0 1}
    set east {1 0}

    set current {0 0}
    set vertices [list $current]

    foreach line $input_lines {
        set parts [split $line]
        set color [lindex $parts 2]
        set dir_input [string index $color 7]
        set length_str [string range $color 2 6]
        scan $length_str %x length

        switch $dir_input {
            3 {set direction $north}
            2 {set direction $west}
            1 {set direction $south}
            0 {set direction $east}
        }

        lassign $current x y
        lassign $direction dx dy
        set new_coord [coord_add $x $y [lindex [coord_multiply_by_scalar $dx $dy $length] 0] [lindex [coord_multiply_by_scalar $dx $dy $length] 1]]
        set vertices [lappend vertices $new_coord]
        set current $new_coord
    }

    return $vertices
}

proc shoelace {vertices} {
    set n [llength $vertices]
    set area 0

    for {set i 0} {$i < $n} {incr i} {
        set next_idx [expr {($i + 1) % $n}]
        lassign [lindex $vertices $i] x1 y1
        lassign [lindex $vertices $next_idx] x2 y2
        set area [expr {$area + $x1 * $y2 - $y1 * $x2}]
    }

    return [expr {[abs_val $area] / 2}]
}

proc perimeter {vertices} {
    set n [llength $vertices]
    set perim 0

    for {set i 0} {$i < $n} {incr i} {
        set next_idx [expr {($i + 1) % $n}]
        lassign [lindex $vertices $i] x1 y1
        lassign [lindex $vertices $next_idx] x2 y2
        set perim [expr {$perim + [abs_val [expr {$x1 - $x2}]] + [abs_val [expr {$y1 - $y2}]]}]
    }

    return $perim
}

proc calculate_polygon_area {vertices} {
    return [expr {[shoelace $vertices] + [perimeter $vertices] / 2 + 1}]
}

proc solve {input_lines} {
    set vertices [parse_input $input_lines]
    set res [calculate_polygon_area $vertices]
    return $res
}

set file [open "input.txt" r]
set input_lines [split [read $file] \n]
close $file

puts [solve $input_lines]
