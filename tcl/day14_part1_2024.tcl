
proc main {} {
    set width 101
    set height 103
    set robots [list]

    set fileId [open "input.txt" r]
    while {[gets $fileId line] != -1} {
        set parts [split $line " "]
        set p_part [lindex $parts 0]
        set v_part [lindex $parts 1]
        set p_coords [string trimright [string trimleft $p_part "p=<"] ">"]
        set v_coords [string trimright [string trimleft $v_part "v=<"] ">"]
        set pos [split $p_coords ","]
        set vel [split $v_coords ","]
        set x [lindex $pos 0]
        set y [lindex $pos 1]
        set vx [lindex $vel 0]
        set vy [lindex $vel 1]
        lappend robots [list $x $y $vx $vy]
    }
    close $fileId

    set num_steps 100
    for {set step 0} {$step < $num_steps} {incr step} {
        for {set i 0} {$i < [llength $robots]} {incr i} {
            set robot [lindex $robots $i]
            set x [lindex $robot 0]
            set y [lindex $robot 1]
            set vx [lindex $robot 2]
            set vy [lindex $robot 3]
            set x_new [expr {(( $x + $vx ) % $width + $width) % $width}]
            set y_new [expr {(( $y + $vy ) % $height + $height) % $height}]
            set robots [lreplace $robots $i $i [list $x_new $y_new $vx $vy]]
        }
    }

    set q1 0
    set q2 0
    set q3 0
    set q4 0
    set center_x 50
    set center_y 51

    foreach robot $robots {
        set x [lindex $robot 0]
        set y [lindex $robot 1]

        if {$x == $center_x || $y == $center_y} {
            continue
        }

        if {$x < $center_x && $y < $center_y} {
            incr q1
        } elseif {$x > $center_x && $y < $center_y} {
            incr q2
        } elseif {$x < $center_x && $y > $center_y} {
            incr q3
        } else {
            incr q4
        }
    }

    puts [expr {$q1 * $q2 * $q3 * $q4}]
}

main
