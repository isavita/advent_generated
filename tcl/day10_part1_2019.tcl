
proc get_angle {a b} {
    return [expr {atan2([lindex $b 1] - [lindex $a 1], [lindex $b 0] - [lindex $a 0])}]
}

proc count_visible_asteroids {station asteroids} {
    set angles {}
    foreach asteroid $asteroids {
        if {$asteroid ne $station} {
            set angle [get_angle $station $asteroid]
            if {![dict exists $angles $angle]} {
                dict set angles $angle 1
            }
        }
    }
    return [dict size $angles]
}

proc find_best_location {asteroids} {
    set max_visible 0
    foreach station $asteroids {
        set visible [count_visible_asteroids $station $asteroids]
        if {$visible > $max_visible} {
            set max_visible $visible
        }
    }
    return $max_visible
}

proc main {} {
    set file [open "input.txt" r]
    set lines [split [read $file] "\n"]
    close $file

    set asteroids {}
    for {set y 0} {$y < [llength $lines]} {incr y} {
        set line [lindex $lines $y]
        for {set x 0} {$x < [string length $line]} {incr x} {
            if {[string index $line $x] eq "#"} {
                lappend asteroids [list $x $y]
            }
        }
    }
    
    set result [find_best_location $asteroids]
    puts $result
}

main
