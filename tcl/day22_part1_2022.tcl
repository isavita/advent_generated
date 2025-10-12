proc read_input {filename} {
    set f [open $filename r]
    set lines [split [read $f] "\n"]
    close $f
    
    set map_lines {}
    set path ""
    set map_section 1
    foreach line $lines {
        if {$line eq ""} {
            set map_section 0
            continue
        }
        if {$map_section} {
            lappend map_lines $line
        } else {
            set path [string trim $line]
        }
    }
    return [list $map_lines $path]
}

proc parse_map {map_lines} {
    set max_width 0
    foreach line $map_lines {
        set len [string length $line]
        if {$len > $max_width} {
            set max_width $len
        }
    }
    
    set grid {}
    foreach line $map_lines {
        lappend grid [string range $line[format %*s $max_width ""] 0 [expr {$max_width - 1}]]
    }
    
    set num_rows [llength $grid]
    set num_cols $max_width
    
    set row_boundaries {}
    for {set y 1} {$y <= $num_rows} {incr y} {
        set row [lindex $grid [expr {$y - 1}]]
        set min_x ""
        set max_x ""
        for {set x 1} {$x <= $num_cols} {incr x} {
            set tile [string index $row [expr {$x - 1}]]
            if {$tile ne " "} {
                if {$min_x eq ""} {
                    set min_x $x
                }
                set max_x $x
            }
        }
        dict set row_boundaries $y [list $min_x $max_x]
    }
    
    set col_boundaries {}
    for {set x 1} {$x <= $num_cols} {incr x} {
        set min_y ""
        set max_y ""
        for {set y 1} {$y <= $num_rows} {incr y} {
            set row [lindex $grid [expr {$y - 1}]]
            set tile [string index $row [expr {$x - 1}]]
            if {$tile ne " "} {
                if {$min_y eq ""} {
                    set min_y $y
                }
                set max_y $y
            }
        }
        dict set col_boundaries $x [list $min_y $max_y]
    }
    
    return [list $grid $row_boundaries $col_boundaries $num_rows $num_cols]
}

proc parse_path {path_str} {
    set instructions {}
    set num ""
    foreach char [split $path_str ""] {
        if {[string is digit $char]} {
            append num $char
        } else {
            if {$num ne ""} {
                lappend instructions $num
                set num ""
            }
            lappend instructions $char
        }
    }
    if {$num ne ""} {
        lappend instructions $num
    }
    return $instructions
}

proc find_starting_position {grid} {
    set top_row [lindex $grid 0]
    for {set x 1} {$x <= [string length $top_row]} {incr x} {
        set tile [string index $top_row [expr {$x - 1}]]
        if {$tile eq "."} {
            return [list $x 1]
        }
    }
    error "No starting position found."
}

proc turn_direction {current_facing turn} {
    if {$turn eq "R"} {
        return [expr {($current_facing + 1) % 4}]
    } elseif {$turn eq "L"} {
        return [expr {($current_facing - 1) % 4}]
    } else {
        error "Invalid turn instruction."
    }
}

proc move {position direction} {
    lassign $position x y
    switch $direction {
        0 { return [list [expr {$x + 1}] $y] }
        1 { return [list $x [expr {$y + 1}]] }
        2 { return [list [expr {$x - 1}] $y] }
        3 { return [list $x [expr {$y - 1}]] }
        default { error "Invalid direction." }
    }
}

proc simulate {grid row_boundaries col_boundaries num_rows num_cols instructions} {
    lassign [find_starting_position $grid] current_x current_y
    set facing 0
    
    foreach instr $instructions {
        if {[string is digit $instr]} {
            set steps $instr
            for {set i 0} {$i < $steps} {incr i} {
                lassign [move [list $current_x $current_y] $facing] next_x next_y
                
                switch $facing {
                    0 {
                        if {$next_x > [lindex [dict get $row_boundaries $current_y] 1]} {
                            set next_x [lindex [dict get $row_boundaries $current_y] 0]
                        }
                    }
                    2 {
                        if {$next_x < [lindex [dict get $row_boundaries $current_y] 0]} {
                            set next_x [lindex [dict get $row_boundaries $current_y] 1]
                        }
                    }
                    1 {
                        if {$next_y > [lindex [dict get $col_boundaries $current_x] 1]} {
                            set next_y [lindex [dict get $col_boundaries $current_x] 0]
                        }
                    }
                    3 {
                        if {$next_y < [lindex [dict get $col_boundaries $current_x] 0]} {
                            set next_y [lindex [dict get $col_boundaries $current_x] 1]
                        }
                    }
                }
                
                set tile [string index [lindex $grid [expr {$next_y - 1}]] [expr {$next_x - 1}]]
                if {$tile eq "#"} {
                    break
                } elseif {$tile eq "."} {
                    set current_x $next_x
                    set current_y $next_y
                } else {
                    error "Invalid tile encountered."
                }
            }
        } elseif {$instr in {"L" "R"}} {
            set facing [turn_direction $facing $instr]
        } else {
            error "Unknown instruction."
        }
    }
    
    return [list $current_x $current_y $facing]
}

proc compute_password {final_position} {
    lassign $final_position x y facing
    return [expr {1000 * $y + 4 * $x + $facing}]
}

proc main {} {
    lassign [read_input "input.txt"] map_lines path_str
    lassign [parse_map $map_lines] grid row_boundaries col_boundaries num_rows num_cols
    set instructions [parse_path $path_str]
    set final_position [simulate $grid $row_boundaries $col_boundaries $num_rows $num_cols $instructions]
    puts [compute_password $final_position]
}

main