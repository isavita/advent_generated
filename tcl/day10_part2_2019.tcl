
proc readAsteroids {filename} {
    set file [open $filename r]
    set grid [list]
    while {[gets $file line] >= 0} {
        set row [list]
        foreach char [split $line ""] {
            lappend row [expr {$char eq "#"}]
        }
        lappend grid $row
    }
    close $file
    return $grid
}

proc countVisibleAsteroids {grid sx sy} {
    set angles [dict create]
    set grid_height [llength $grid]
    set grid_width [llength [lindex $grid 0]]

    for {set ty 0} {$ty < $grid_height} {incr ty} {
        for {set tx 0} {$tx < $grid_width} {incr tx} {
            if {[lindex [lindex $grid $ty] $tx] && !($tx == $sx && $ty == $sy)} {
                set dy [expr {$ty - $sy}]
                set dx [expr {$tx - $sx}]
                set angle [expr {atan2($dy, $dx)}]
                dict set angles $angle 1
            }
        }
    }
    return [dict size $angles]
}

proc findBestAsteroidLocation {grid} {
    set best_x -1
    set best_y -1
    set max_visible -1
    set grid_height [llength $grid]
    set grid_width [llength [lindex $grid 0]]

    for {set y 0} {$y < $grid_height} {incr y} {
        for {set x 0} {$x < $grid_width} {incr x} {
            if {[lindex [lindex $grid $y] $x]} {
                set count [countVisibleAsteroids $grid $x $y]
                if {$count > $max_visible} {
                    set max_visible $count
                    set best_x $x
                    set best_y $y
                }
            }
        }
    }
    return [list $best_x $best_y]
}

proc vaporizeAsteroids {grid station_x station_y} {
    set targets_by_angle [dict create]
    set grid_height [llength $grid]
    set grid_width [llength [lindex $grid 0]]
    set PI [expr {acos(-1.0)}]

    for {set ty 0} {$ty < $grid_height} {incr ty} {
        for {set tx 0} {$tx < $grid_width} {incr tx} {
            if {[lindex [lindex $grid $ty] $tx] && !($tx == $station_x && $ty == $station_y)} {
                set dy [expr {$ty - $station_y}]
                set dx [expr {$tx - $station_x}]
                set dist [expr {hypot($dx, $dy)}]
                set raw_angle [expr {atan2($dy, $dx)}]
                set angle [expr {$raw_angle + $PI / 2.0}]
                if {$angle < 0} {
                    set angle [expr {$angle + 2 * $PI}]
                }

                set ast_info [list $dist $tx $ty]
                dict lappend targets_by_angle $angle $ast_info
            }
        }
    }

    foreach angle [dict keys $targets_by_angle] {
        set sorted_asts [lsort -real -index 0 [dict get $targets_by_angle $angle]]
        dict set targets_by_angle $angle $sorted_asts
    }

    set sorted_angles [lsort -real [dict keys $targets_by_angle]]

    set vaporized [list]
    set vaporized_count 0

    while {$vaporized_count < 200} {
        set vaporized_in_round false
        foreach angle $sorted_angles {
            if {[dict exists $targets_by_angle $angle] && [llength [dict get $targets_by_angle $angle]] > 0} {
                set ast_to_vaporize [lindex [dict get $targets_by_angle $angle] 0]
                lappend vaporized $ast_to_vaporize
                incr vaporized_count
                set vaporized_in_round true

                set remaining_asts [lrange [dict get $targets_by_angle $angle] 1 end]
                if {[llength $remaining_asts] == 0} {
                    dict unset targets_by_angle $angle
                } else {
                    dict set targets_by_angle $angle $remaining_asts
                }

                if {$vaporized_count >= 200} { break }
            }
        }
        if {!$vaporized_in_round} { break }
    }

    return $vaporized
}

set grid [readAsteroids "input.txt"]
set station [findBestAsteroidLocation $grid]
set station_x [lindex $station 0]
set station_y [lindex $station 1]

set vaporized [vaporizeAsteroids $grid $station_x $station_y]

if {[llength $vaporized] >= 200} {
    set ast_200 [lindex $vaporized 199]
    set ast_x [lindex $ast_200 1]
    set ast_y [lindex $ast_200 2]
    set result [expr {$ast_x * 100 + $ast_y}]
    puts $result
}
