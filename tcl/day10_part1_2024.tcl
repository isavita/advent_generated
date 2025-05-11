
proc main {} {
    set fileId [open "input.txt" r]
    set data [read $fileId]
    close $fileId
    set lines [split [string trim $data] "\n"]

    set grid [list]
    set nr 0
    foreach line $lines {
        set row [list]
        foreach char [split $line ""] {
            lappend row [expr {$char}]
        }
        lappend grid $row
        incr nr
    }
    if {$nr > 0} {
        set nc [llength [lindex $grid 0]]
    } else {
        set nc 0
    }

    set trailheads [list]
    for {set r 0} {$r < $nr} {incr r} {
        for {set c 0} {$c < $nc} {incr c} {
            if {[lindex [lindex $grid $r] $c] == 0} {
                lappend trailheads [list $r $c]
            }
        }
    }

    set dirs {{1 0} {-1 0} {0 1} {0 -1}}
    set sum_scores 0

    foreach th $trailheads {
        set reached [dict create]
        set front [list]
        set visited [dict create]

        set start_r [lindex $th 0]
        set start_c [lindex $th 1]

        lappend front [list $th 0]
        dict set visited "$start_r,$start_c,0" 1

        while {[llength $front] > 0} {
            set state [lindex $front 0]
            set front [lrange $front 1 end]

            set cur_coords [lindex $state 0]
            set r [lindex $cur_coords 0]
            set c [lindex $cur_coords 1]
            set h [lindex $state 1]

            if {$h == 9} {
                dict set reached "$r,$c" 1
                continue
            }

            set next_h [expr {$h + 1}]
            foreach dir $dirs {
                set dr [lindex $dir 0]
                set dc [lindex $dir 1]
                set nr2 [expr {$r + $dr}]
                set nc2 [expr {$c + $dc}]

                if {$nr2 >= 0 && $nr2 < $nr && $nc2 >= 0 && $nc2 < $nc} {
                    set neighbor_value [lindex [lindex $grid $nr2] $nc2]

                    if {$neighbor_value == $next_h} {
                        set state_key "$nr2,$nc2,$next_h"

                        if {![dict exists $visited $state_key]} {
                            dict set visited $state_key 1
                            lappend front [list [list $nr2 $nc2] $next_h]
                        }
                    }
                }
            }
        }
        set sum_scores [expr {$sum_scores + [dict size $reached]}]
    }

    puts $sum_scores
}

main
