
set rotations {
    {{1 0 0} {0 1 0} {0 0 1}}
    {{1 0 0} {0 0 -1} {0 1 0}}
    {{1 0 0} {0 -1 0} {0 0 -1}}
    {{1 0 0} {0 0 1} {0 -1 0}}
    {{0 -1 0} {1 0 0} {0 0 1}}
    {{0 0 1} {1 0 0} {0 1 0}}
    {{0 1 0} {1 0 0} {0 0 -1}}
    {{0 0 -1} {1 0 0} {0 -1 0}}
    {{-1 0 0} {0 -1 0} {0 0 1}}
    {{-1 0 0} {0 0 -1} {0 -1 0}}
    {{-1 0 0} {0 1 0} {0 0 -1}}
    {{-1 0 0} {0 0 1} {0 1 0}}
    {{0 1 0} {-1 0 0} {0 0 1}}
    {{0 0 1} {-1 0 0} {0 -1 0}}
    {{0 -1 0} {-1 0 0} {0 0 -1}}
    {{0 0 -1} {-1 0 0} {0 1 0}}
    {{0 0 -1} {0 1 0} {1 0 0}}
    {{0 1 0} {0 0 1} {1 0 0}}
    {{0 0 1} {0 -1 0} {1 0 0}}
    {{0 -1 0} {0 0 -1} {1 0 0}}
    {{0 0 -1} {0 -1 0} {-1 0 0}}
    {{0 -1 0} {0 0 1} {-1 0 0}}
    {{0 0 1} {0 1 0} {-1 0 0}}
    {{0 1 0} {0 0 -1} {-1 0 0}}
}

proc parse_point {s} {
    return [split $s ,]
}

proc point_to_string {p} {
    return [join $p ,]
}

proc vec_add {p1 p2} {
    return [list \
        [expr {[lindex $p1 0] + [lindex $p2 0]}] \
        [expr {[lindex $p1 1] + [lindex $p2 1]}] \
        [expr {[lindex $p1 2] + [lindex $p2 2]}] \
    ]
}

proc vec_subtract {p1 p2} {
    return [list \
        [expr {[lindex $p1 0] - [lindex $p2 0]}] \
        [expr {[lindex $p1 1] - [lindex $p2 1]}] \
        [expr {[lindex $p1 2] - [lindex $p2 2]}] \
    ]
}

proc apply_rotation {p rot_matrix} {
    set rotated_p [list 0 0 0]
    for {set i 0} {$i < 3} {incr i} {
        set sum 0
        for {set j 0} {$j < 3} {incr j} {
            set sum [expr {$sum + [lindex $rot_matrix $i $j] * [lindex $p $j]}]
        }
        lset rotated_p $i $sum
    }
    return $rotated_p
}

proc read_input {filename} {
    set scanners [list]
    set current_scanner [list]
    set f [open $filename r]
    while {[gets $f line] >= 0} {
        set line [string trim $line]
        if {[string match "--- scanner*" $line]} {
            if {[llength $current_scanner] > 0} {
                lappend scanners $current_scanner
            }
            set current_scanner [list]
        } elseif {$line ne ""} {
            lappend current_scanner [parse_point $line]
        }
    }
    if {[llength $current_scanner] > 0} {
        lappend scanners $current_scanner
    }
    close $f
    return $scanners
}

set scanners [read_input "input.txt"]

set unique_beacons [dict create]
set scanner_positions [list {0 0 0}]

set initial_scanner0_beacons [lindex $scanners 0]
set scanners [lreplace $scanners 0 0]

foreach beacon $initial_scanner0_beacons {
    dict set unique_beacons [point_to_string $beacon] 1
}

while {[llength $scanners] > 0} {
    set found_match false
    for {set i [expr {[llength $scanners] - 1}]} {$i >= 0} {incr i -1} {
        set current_scanner [lindex $scanners $i]

        foreach rotation $::rotations {
            set rotated_beacons [list]
            foreach beacon $current_scanner {
                lappend rotated_beacons [apply_rotation $beacon $rotation]
            }

            set offset_counts [dict create]
            dict for {beacon1_str value} $unique_beacons {
                 set beacon1 [parse_point $beacon1_str]
                 foreach beacon2 $rotated_beacons {
                     set offset [vec_subtract $beacon1 $beacon2]
                     set offset_str [point_to_string $offset]
                     dict incr offset_counts $offset_str
                 }
             }

            set matched_offset ""
            dict for {offset_str count} $offset_counts {
                if {$count >= 12} {
                    set matched_offset $offset_str
                    break
                }
            }

            if {$matched_offset ne ""} {
                set scanner_pos [parse_point $matched_offset]
                lappend scanner_positions $scanner_pos

                foreach rotated_beacon $rotated_beacons {
                    set translated_beacon [vec_add $rotated_beacon $scanner_pos]
                    dict set unique_beacons [point_to_string $translated_beacon] 1
                }

                set scanners [lreplace $scanners $i $i]

                set found_match true
                break
            }
        }
        if {$found_match} {
            break
        }
    }
}

puts [dict size $unique_beacons]
