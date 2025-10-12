proc Coord {x y} {
    return [list $x $y]
}

proc add {a b} {
    lassign $a ax ay
    lassign $b bx by
    return [Coord [expr {$ax + $bx}] [expr {$ay + $by}]]
}

proc subtract {a b} {
    lassign $a ax ay
    lassign $b bx by
    return [Coord [expr {$ax - $bx}] [expr {$ay - $by}]]
}

proc opposite {c} {
    lassign $c x y
    return [Coord [expr {-$x}] [expr {-$y}]]
}

set Undefined [Coord 0 0]
set Top [Coord 0 -1]
set Right [Coord 1 0]
set Bottom [Coord 0 1]
set Left [Coord -1 0]

set Empty "."
set Start "S"
set Vertical "|"
set Horizontal "-"
set TopLeftCorner "J"
set TopRightCorner "L"
set BottomLeftCorner "7"
set BottomRightCorner "F"
set Enclosed "X"

set VerticalPipe [dict create $Top {} $Bottom {}]
set HorizontalPipe [dict create $Left {} $Right {}]
set TopLeftCornerPipe [dict create $Top {} $Left {}]
set TopRightCornerPipe [dict create $Top {} $Right {}]
set BottomLeftCornerPipe [dict create $Bottom {} $Left {}]
set BottomRightCornerPipe [dict create $Bottom {} $Right {}]

set TileToPipe [dict create \
    $Vertical $VerticalPipe \
    $Horizontal $HorizontalPipe \
    $TopLeftCorner $TopLeftCornerPipe \
    $TopRightCorner $TopRightCornerPipe \
    $BottomLeftCorner $BottomLeftCornerPipe \
    $BottomRightCorner $BottomRightCornerPipe \
]

proc get_pipe_from_tile {tile} {
    global TileToPipe
    if {[dict exists $TileToPipe $tile]} {
        return [dict get $TileToPipe $tile]
    }
    return [dict create]
}

proc get_tile_from_pipe {pipe} {
    global TileToPipe Empty
    dict for {tile associated_pipe} $TileToPipe {
        if {[is_equal_pipe $pipe $associated_pipe]} {
            return $tile
        }
    }
    return $Empty
}

proc is_equal_pipe {pipe1 pipe2} {
    if {[dict size $pipe1] != [dict size $pipe2]} {
        return 0
    }
    dict for {dir _} $pipe1 {
        if {![dict exists $pipe2 $dir]} {
            return 0
        }
    }
    return 1
}

proc build_grid {input_lines} {
    set grid [dict create]
    set y 0
    foreach line $input_lines {
        set x 0
        foreach char [split $line ""] {
            if {$char != "."} {
                dict set grid [Coord $x $y] $char
            }
            incr x
        }
        incr y
    }
    return $grid
}

proc find_start {grid} {
    dict for {coord value} $grid {
        if {$value == "S"} {
            return $coord
        }
    }
    return [Coord 0 0]
}

proc get_pipe_from_neighbors {coord grid} {
    global Top Right Bottom Left
    set pipe [dict create]
    set possible_neighbors [list \
        [list $Top [add $coord $Top]] \
        [list $Right [add $coord $Right]] \
        [list $Bottom [add $coord $Bottom]] \
        [list $Left [add $coord $Left]] \
    ]
    foreach pair $possible_neighbors {
        lassign $pair dir neighbor_coord
        if {[dict exists $grid $neighbor_coord]} {
            set neighbor_pipe [get_pipe_from_tile [dict get $grid $neighbor_coord]]
            if {[dict exists $neighbor_pipe [opposite $dir]]} {
                dict set pipe $dir {}
            }
        }
    }
    return $pipe
}

proc path_finding {start grid} {
    set path [list $start]
    set start_pipe [get_pipe_from_neighbors $start $grid]
    set previous_dir {}
    set current {}
    dict for {dir _} $start_pipe {
        set previous_dir $dir
        set current [add $start $dir]
        break
    }
    while {$current != $start} {
        lappend path $current
        set current_pipe [get_pipe_from_tile [dict get $grid $current]]
        dict for {dir _} $current_pipe {
            if {$dir != [opposite $previous_dir]} {
                set previous_dir $dir
                set current [add $current $dir]
                break
            }
        }
    }
    return $path
}

proc get_path_grid {grid path empty} {
    set new_grid [dict create]
    foreach coord $path {
        dict set new_grid $coord [dict get $grid $coord]
    }
    set start [lindex $path 0]
    dict set new_grid $start [get_tile_from_pipe [get_pipe_from_neighbors $start $grid]]
    return $new_grid
}

proc is_inside {coord grid empty} {
    if {[dict exists $grid $coord]} {
        return 0
    }
    set start_pipe $empty
    set num_pipe_on_left 0
    lassign $coord cx cy
    for {set x 0} {$x < $cx} {incr x} {
        set c [Coord $x $cy]
        if {[dict exists $grid $c]} {
            set v [dict get $grid $c]
            if {$v == "|"} {
                incr num_pipe_on_left
            } elseif {$v == "L"} {
                set start_pipe "L"
            } elseif {$v == "F"} {
                set start_pipe "F"
            } elseif {$v == "J"} {
                if {$start_pipe == "F"} {
                    set start_pipe $empty
                    incr num_pipe_on_left
                } elseif {$start_pipe == "L"} {
                    set start_pipe $empty
                }
            } elseif {$v == "7"} {
                if {$start_pipe == "L"} {
                    set start_pipe $empty
                    incr num_pipe_on_left
                } elseif {$start_pipe == "F"} {
                    set start_pipe $empty
                }
            }
        }
    }
    return [expr {$num_pipe_on_left % 2 == 1}]
}

proc solve {input_lines} {
    set grid [build_grid $input_lines]
    set start [find_start $grid]
    set path [path_finding $start $grid]
    set path_grid [get_path_grid $grid $path "."]
    set count 0
    set height [llength $input_lines]
    set width [string length [lindex $input_lines 0]]
    for {set y 0} {$y < $height} {incr y} {
        for {set x 0} {$x < $width} {incr x} {
            set c [Coord $x $y]
            if {[is_inside $c $path_grid "."]} {
                incr count
            }
        }
    }
    return $count
}

proc read_file {file_name} {
    set lines {}
    set f [open $file_name r]
    while {[gets $f line] >= 0} {
        lappend lines [string trim $line]
    }
    close $f
    return $lines
}

set input_lines [read_file "input.txt"]
puts [solve $input_lines]