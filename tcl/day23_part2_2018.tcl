
proc parse_input {filename} {
    set nanobots {}
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        if {[regexp {pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)} $line -> x y z r]} {
            lappend nanobots [list $x $y $z $r]
        }
    }
    close $file
    return $nanobots
}

proc manhattan_distance {a b} {
    return [expr {abs([lindex $a 0] - [lindex $b 0]) + abs([lindex $a 1] - [lindex $b 1]) + abs([lindex $a 2] - [lindex $b 2])}]
}

proc part_one {nanobots} {
    set strongest {}
    set max_r -1
    foreach bot $nanobots {
        set r [lindex $bot 3]
        if {$r > $max_r} {
            set max_r $r
            set strongest $bot
        }
    }
    
    set sx [lindex $strongest 0]
    set sy [lindex $strongest 1]
    set sz [lindex $strongest 2]
    set sr [lindex $strongest 3]
    
    set count 0
    foreach bot $nanobots {
        set distance [manhattan_distance [list $sx $sy $sz] [lrange $bot 0 2]]
        if {$distance <= $sr} {
            incr count
        }
    }
    return $count
}

proc min_distance_to_origin {x y z size} {
    set dx 0
    if {$x > 0} {
        set dx $x
    } elseif {$x + $size - 1 < 0} {
        set dx [expr {-($x + $size - 1)}]
    }

    set dy 0
    if {$y > 0} {
        set dy $y
    } elseif {$y + $size - 1 < 0} {
        set dy [expr {-($y + $size - 1)}]
    }

    set dz 0
    if {$z > 0} {
        set dz $z
    } elseif {$z + $size - 1 < 0} {
        set dz [expr {-($z + $size - 1)}]
    }

    return [expr {$dx + $dy + $dz}]
}

proc part_two {nanobots} {
    set min_x [lindex [lindex $nanobots 0] 0]
    set max_x [lindex [lindex $nanobots 0] 0]
    set min_y [lindex [lindex $nanobots 0] 1]
    set max_y [lindex [lindex $nanobots 0] 1]
    set min_z [lindex [lindex $nanobots 0] 2]
    set max_z [lindex [lindex $nanobots 0] 2]

    foreach bot $nanobots {
        set x [lindex $bot 0]
        set y [lindex $bot 1]
        set z [lindex $bot 2]

        if {$x < $min_x} {set min_x $x}
        if {$x > $max_x} {set max_x $x}
        if {$y < $min_y} {set min_y $y}
        if {$y > $max_y} {set max_y $y}
        if {$z < $min_z} {set min_z $z}
        if {$z > $max_z} {set max_z $z}
    }

    set size 1
    while {$size < [expr {max($max_x - $min_x, $max_y - $min_y, $max_z - $min_z)}]} {
        set size [expr {$size * 2}]
    }

    set heap [list [list 0 [min_distance_to_origin $min_x $min_y $min_z $size] $size $min_x $min_y $min_z]]

    set best_distance {}
    set best_count -1

    while {[llength $heap] > 0} {
        # Simple heap pop (no proper heap implementation)
        set best_index 0
        for {set i 1} {$i < [llength $heap]} {incr i} {
            if {[lindex [lindex $heap $i] 0] < [lindex [lindex $heap $best_index] 0]} {
                set best_index $i
            } elseif {[lindex [lindex $heap $i] 0] == [lindex [lindex $heap $best_index] 0] && [lindex [lindex $heap $i] 1] < [lindex [lindex $heap $best_index] 1]} {
                set best_index $i
            }
        }
        set current_item [lindex $heap $best_index]
        set heap [lreplace $heap $best_index $best_index]

        set neg_count [lindex $current_item 0]
        set distance [lindex $current_item 1]
        set size [lindex $current_item 2]
        set x [lindex $current_item 3]
        set y [lindex $current_item 4]
        set z [lindex $current_item 5]
        set count [expr {-$neg_count}]

        if {$size == 1} {
            if {$count > $best_count || ($count == $best_count && [string compare $distance $best_distance] < 0)} {
                set best_count $count
                set best_distance $distance
                break
            }
            continue
        }

        set half [expr {$size / 2}]
        foreach dx [list 0 $half] {
            foreach dy [list 0 $half] {
                foreach dz [list 0 $half] {
                    set nx [expr {$x + $dx}]
                    set ny [expr {$y + $dy}]
                    set nz [expr {$z + $dz}]
                    set new_size $half
                    if {$new_size < 1} {
                        set new_size 1
                    }

                    set count 0
                    foreach bot $nanobots {
                        set bx [lindex $bot 0]
                        set by [lindex $bot 1]
                        set bz [lindex $bot 2]
                        set br [lindex $bot 3]

                        set d 0
                        if {$bx < $nx} {
                            set d [expr {$d + $nx - $bx}]
                        } elseif {$bx > $nx + $new_size - 1} {
                            set d [expr {$d + $bx - ($nx + $new_size - 1)}]
                        }
                        if {$by < $ny} {
                            set d [expr {$d + $ny - $by}]
                        } elseif {$by > $ny + $new_size - 1} {
                            set d [expr {$d + $by - ($ny + $new_size - 1)}]
                        }
                        if {$bz < $nz} {
                            set d [expr {$d + $nz - $bz}]
                        } elseif {$bz > $nz + $new_size - 1} {
                            set d [expr {$d + $bz - ($nz + $new_size - 1)}]
                        }
                        if {$d <= $br} {
                            incr count
                        }
                    }

                    set distance [min_distance_to_origin $nx $ny $nz $new_size]

                    lappend heap [list [expr {-$count}] $distance $new_size $nx $ny $nz]
                }
            }
        }
    }

    return $best_distance
}

proc main {} {
    set input_file "input.txt"
    set nanobots [parse_input $input_file]

    set count_in_range [part_one $nanobots]
    puts "Part One: $count_in_range"

    set shortest_distance [part_two $nanobots]
    puts "Part Two: $shortest_distance"
}

main
