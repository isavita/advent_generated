#!/usr/bin/env tclsh
# Helpers
proc repeat {char count} {
    set s ""
    for {set i 0} {$i < $count} {incr i} {
        append s $char
    }
    return $s
}
proc reverse_string {s} {
    set out ""
    set len [string length $s]
    for {set i [expr {$len - 1}]} {$i >= 0} {incr i -1} {
        append out [string index $s $i]
    }
    return $out
}
proc rotate_pattern {pattern} {
    set rows [split $pattern "/"]
    set size [llength $rows]
    set pieces {}
    for {set x 0} {$x < $size} {incr x} {
        set col ""
        for {set y [expr {$size - 1}]} {$y >= 0} {incr y -1} {
            set ch [string index [lindex $rows $y] $x]
            append col $ch
        }
        lappend pieces $col
    }
    return [join $pieces "/"]
}
proc flip_pattern {pattern} {
    set rows [split $pattern "/"]
    set out_rows {}
    foreach r $rows {
        lappend out_rows [reverse_string $r]
    }
    return [join $out_rows "/"]
}
proc extract_subsquare {grid y x subsize} {
    set pieces {}
    for {set dy 0} {$dy < $subsize} {incr dy} {
        set row [lindex $grid [expr {$y + $dy}]]
        set end_index [expr {$x + $subsize - 1}]
        set seg [string range $row $x $end_index]
        lappend pieces $seg
    }
    return [join $pieces "/"]
}
proc enhance {pattern} {
    global rules memo
    if {[dict exists $memo $pattern]} {
        return [dict get $memo $pattern]
    }
    set current $pattern
    for {set i 0} {$i < 4} {incr i} {
        if {[dict exists $rules $current]} {
            set result [dict get $rules $current]
            dict set memo $pattern $result
            return $result
        }
        set current [rotate_pattern $current]
    }
    set flipped [flip_pattern $pattern]
    set current $flipped
    for {set i 0} {$i < 4} {incr i} {
        if {[dict exists $rules $current]} {
            set result [dict get $rules $current]
            dict set memo $pattern $result
            return $result
        }
        set current [rotate_pattern $current]
    }
    puts stderr "Error: No rule for pattern $pattern"
    exit 1
}
proc main {} {
    global rules memo grid

    set rules {}
    set memo {}

    set fh [open "input.txt" r]
    while {[gets $fh line] >= 0} {
        if {$line eq ""} { continue }
        if { [regexp {^(.*) => (.*)$} $line -> input_pattern output_pattern] } {
            dict set rules $input_pattern $output_pattern
        }
    }
    close $fh

    set grid [list ".#." "..#" "###"]

    for {set iter 0} {$iter < 18} {incr iter} {
        set grid_size [llength $grid]
        if { ($grid_size % 2) == 0 } {
            set sub_size 2
            set new_sub_size 3
        } else {
            set sub_size 3
            set new_sub_size 4
        }

        set blocks [expr {$grid_size / $sub_size}]
        set new_grid_size [expr {$blocks * $new_sub_size}]

        set new_grid [list]
        for {set i 0} {$i < $new_grid_size} {incr i} {
            lappend new_grid [repeat "." $new_grid_size]
        }

        for {set y 0} {$y < $grid_size} {incr y $sub_size} {
            for {set x 0} {$x < $grid_size} {incr x $sub_size} {
                set sub_pat [extract_subsquare $grid $y $x $sub_size]
                set enhanced [enhance $sub_pat]

                set block_y [expr {$y / $sub_size}]
                set new_y_base [expr {$block_y * $new_sub_size}]
                set block_x [expr {$x / $sub_size}]
                set new_x_base [expr {$block_x * $new_sub_size}]

                set rows_pat [split $enhanced "/"]
                for {set dy 0} {$dy < $new_sub_size} {incr dy} {
                    set target_row_index [expr {$new_y_base + $dy}]
                    set current_row [lindex $new_grid $target_row_index]

                    set left_len [expr {$new_x_base}]
                    set left ""
                    if {$left_len > 0} {
                        set left [string range $current_row 0 [expr {$new_x_base - 1}]]
                    }

                    set mid_row [lindex $rows_pat $dy]

                    set right_start [expr {$new_x_base + $new_sub_size}]
                    set end_index [expr {$new_grid_size - 1}]
                    set right ""
                    if {$right_start <= $end_index} {
                        set right [string range $current_row $right_start $end_index]
                    }

                    set new_row "$left$mid_row$right"
                    set new_grid [lreplace $new_grid $target_row_index $target_row_index $new_row]
                }
            }
        }

        set grid $new_grid
    }

    set count 0
    foreach row $grid {
        set len [string length $row]
        for {set i 0} {$i < $len} {incr i} {
            if {[string index $row $i] eq "#"} {
                incr count
            }
        }
    }
    puts $count
}
main