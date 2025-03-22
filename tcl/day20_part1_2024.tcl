
proc bfs {grid h w start ignore_walls} {
    set dist [lrepeat $h [lrepeat $w -1]]
    lset dist [lindex $start 0] [lindex $start 1] 0
    set q [list $start]
    set dirs [list {1 0} {-1 0} {0 1} {0 -1}]

    while {[llength $q] > 0} {
        set curr [lindex $q 0]
        set q [lrange $q 1 end]
        set r [lindex $curr 0]
        set c [lindex $curr 1]

        foreach dir $dirs {
            set dr [lindex $dir 0]
            set dc [lindex $dir 1]
            set nr [expr {$r + $dr}]
            set nc [expr {$c + $dc}]

            if {$nr >= 0 && $nr < $h && $nc >= 0 && $nc < $w} {
                if {!$ignore_walls && [string equal [lindex [lindex $grid $nr] $nc] "#"]} {
                    continue
                }
                if {[lindex [lindex $dist $nr] $nc] == -1} {
                    lset dist $nr $nc [expr {[lindex [lindex $dist $r] $c] + 1}]
                    lappend q [list $nr $nc]
                }
            }
        }
    }
    return $dist
}

proc solve {} {
    set f [open "input.txt" r]
    set grid [list]
    while {[gets $f line] >= 0} {
        lappend grid [split $line ""]
    }
    close $f

    set h [llength $grid]
    set w [llength [lindex $grid 0]]

    set S ""
    set E ""
    for {set r 0} {$r < $h} {incr r} {
        for {set c 0} {$c < $w} {incr c} {
            set cell [lindex [lindex $grid $r] $c]
            if {[string equal $cell "S"]} {
                set S [list $r $c]
            } elseif {[string equal $cell "E"]} {
                set E [list $r $c]
            }
        }
    }

    set track_cells [list]
    set walls [lrepeat $h [lrepeat $w 0]]
    for {set r 0} {$r < $h} {incr r} {
        for {set c 0} {$c < $w} {incr c} {
            if {[string equal [lindex [lindex $grid $r] $c] "#"]} {
                lset walls $r $c 1
            } else {
                lappend track_cells [list $r $c]
            }
        }
    }

    set dist_from_s [bfs $grid $h $w $S 0]
    set dist_from_e [bfs $grid $h $w $E 0]

    if {[lindex [lindex $dist_from_s [lindex $E 0]] [lindex $E 1]] == -1} {
        puts 0
        return
    }

    set normal_cost [lindex [lindex $dist_from_s [lindex $E 0]] [lindex $E 1]]

    proc is_track {r c h w walls grid} {
        return [expr {$r >= 0 && $r < $h && $c >= 0 && $c < $w && !([lindex [lindex $walls $r] $c])}]
    }
    
    set possible_cheats 0
    set dirs [list {1 0} {-1 0} {0 1} {0 -1}]

    foreach start_pos $track_cells {
        set r [lindex $start_pos 0]
        set c [lindex $start_pos 1]
        set sd [lindex [lindex $dist_from_s $r] $c]
        if {$sd == -1} {
            continue
        }

        foreach dir1 $dirs {
            set dr1 [lindex $dir1 0]
            set dc1 [lindex $dir1 1]
            set m1r [expr {$r + $dr1}]
            set m1c [expr {$c + $dc1}]
            if {$m1r < 0 || $m1r >= $h || $m1c < 0 || $m1c >= $w} {
                continue
            }

            foreach dir2 $dirs {
                set dr2 [lindex $dir2 0]
                set dc2 [lindex $dir2 1]
                set m2r [expr {$m1r + $dr2}]
                set m2c [expr {$m1c + $dc2}]

                if {$m2r < 0 || $m2r >= $h || $m2c < 0 || $m2c >= $w} {
                    continue
                }

                if {![is_track $m2r $m2c $h $w $walls $grid]} {
                    continue
                }
                set ed [lindex [lindex $dist_from_e $m2r] $m2c]
                if {$ed == -1} {
                    continue
                }
                set new_cost [expr {$sd + 2 + $ed}]
                set saving [expr {$normal_cost - $new_cost}]
                if {$saving >= 100} {
                    incr possible_cheats
                }
            }
        }
    }
    puts $possible_cheats
}

solve
