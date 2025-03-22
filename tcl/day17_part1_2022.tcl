
proc read_input {filename} {
    set fp [open $filename r]
    gets $fp line
    close $fp
    return [string trim $line]
}

proc get_rock_shapes {} {
    return [list \
        {0 0 1 0 2 0 3 0} \
        {1 0 0 1 1 1 2 1 1 2} \
        {0 0 1 0 2 0 2 1 2 2} \
        {0 0 0 1 0 2 0 3} \
        {0 0 1 0 0 1 1 1}]
}

proc can_move {rock direction chamber highest_y} {
    foreach {x y} $rock {
        switch -- $direction {
            left {set new_x [expr {$x - 1}]; set new_y $y}
            right {set new_x [expr {$x + 1}]; set new_y $y}
            down {set new_x $x; set new_y [expr {$y - 1}]}
            default {error "Invalid direction"}
        }

        if {$new_x < 0 || $new_x > 6 || $new_y < 1} {
            return 0
        }
        if {[dict exists $chamber "$new_x,$new_y"]} {
            return 0
        }
        lappend moved_rock $new_x $new_y
    }
    return $moved_rock
}

proc simulate {jet_pattern total_rocks} {
    set rock_shapes [get_rock_shapes]
    set chamber [dict create]

    for {set x 0} {$x < 7} {incr x} {
        dict set chamber "$x,0" 1
    }

    set highest_y 0
    set jet_len [string length $jet_pattern]
    set jet_index 0

    for {set rock_number 0} {$rock_number < $total_rocks} {incr rock_number} {
        set shape [lindex $rock_shapes [expr {$rock_number % [llength $rock_shapes]}]]
        set rock_x 2
        set rock_y [expr {$highest_y + 4}]
        set rock {}

        for {set i 0} {$i < [llength $shape]} {incr i 2} {
            set dx [lindex $shape $i]
            set dy [lindex $shape [expr {$i + 1}]]
            lappend rock [expr {$rock_x + $dx}] [expr {$rock_y + $dy}]
        }

        while 1 {
            set jet_dir [string index $jet_pattern [expr {$jet_index % $jet_len}]]
            incr jet_index

            switch -- $jet_dir {
                ">" {set moved_rock [can_move $rock right $chamber $highest_y]}
                "<" {set moved_rock [can_move $rock left $chamber $highest_y]}
                default {error "Invalid jet direction"}
            }

            if {$moved_rock != 0} {
                set rock $moved_rock
            }

            set moved_down [can_move $rock down $chamber $highest_y]
            if {$moved_down != 0} {
                set rock $moved_down
            } else {
                foreach {x y} $rock {
                    dict set chamber "$x,$y" 1
                    if {$y > $highest_y} {
                        set highest_y $y
                    }
                }
                break
            }
        }
    }
    return $highest_y
}

proc main {} {
    set jet_pattern [read_input "input.txt"]
    set total_rocks 2022
    set final_height [simulate $jet_pattern $total_rocks]
    puts $final_height
}

main
