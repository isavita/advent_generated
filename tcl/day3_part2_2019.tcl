
proc read_input {filename} {
    set fh [open $filename r]
    set data [split [read $fh] "\n"]
    close $fh
    return $data
}

proc parse_path {path} {
    set steps [split $path ","]
    set x 0
    set y 0
    set path_points [dict create]
    set step_count 0

    foreach step $steps {
        set direction [string index $step 0]
        set distance [expr {int([string range $step 1 end])}]
        for {set i 0} {$i < $distance} {incr i} {
            incr step_count
            switch $direction {
                R {incr x}
                L {incr x -1}
                U {incr y}
                D {incr y -1}
            }
            if {![dict exists $path_points "$x,$y"]} {
                dict set path_points "$x,$y" $step_count
            }
        }
    }
    return $path_points
}

proc find_intersections {path1 path2} {
    set intersections [dict create]
    foreach {point steps1} $path1 {
        if {[dict exists $path2 $point]} {
            set steps2 [dict get $path2 $point]
            dict set intersections $point [list $steps1 $steps2]
        }
    }
    return $intersections
}

proc calculate_manhattan_distance {point} {
    lassign [split $point ","] x y
    return [expr {abs($x) + abs($y)}]
}

proc calculate_min_steps {intersections} {
    set min_steps inf
    foreach {point steps} $intersections {
        lassign $steps steps1 steps2
        set total_steps [expr {$steps1 + $steps2}]
        if {$total_steps < $min_steps} {
            set min_steps $total_steps
        }
    }
    return $min_steps
}

proc main {} {
    set data [read_input "input.txt"]
    set path1 [parse_path [lindex $data 0]]
    set path2 [parse_path [lindex $data 1]]
    set intersections [find_intersections $path1 $path2]

    set min_distance inf
    foreach point [dict keys $intersections] {
        set distance [calculate_manhattan_distance $point]
        if {$distance < $min_distance && $distance != 0} {
            set min_distance $distance
        }
    }

    set min_steps [calculate_min_steps $intersections]

    puts "Closest intersection Manhattan distance: $min_distance"
    puts "Fewest combined steps to an intersection: $min_steps"
}

main
