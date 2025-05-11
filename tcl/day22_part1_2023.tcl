
proc create_coord {x y z} {
    return [dict create x $x y $y z $z]
}

proc create_brick {mini maxi} {
    return [dict create mini $mini maxi $maxi basedOn [list] support [list]]
}

proc is_intersecting {brick1 brick2} {
    set mini1 [dict get $brick1 mini]
    set maxi1 [dict get $brick1 maxi]
    set mini2 [dict get $brick2 mini]
    set maxi2 [dict get $brick2 maxi]

    set x1_min [dict get $mini1 x]
    set x1_max [dict get $maxi1 x]
    set y1_min [dict get $mini1 y]
    set y1_max [dict get $maxi1 y]

    set x2_min [dict get $mini2 x]
    set x2_max [dict get $maxi2 x]
    set y2_min [dict get $mini2 y]
    set y2_max [dict get $maxi2 y]

    if {$x1_max < $x2_min || $x2_max < $x1_min || $y1_max < $y2_min || $y2_max < $y1_min} {
        return 0
    }
    return 1
}

proc parseInput {lines} {
    set bricks [list]
    foreach line $lines {
        set parts [split $line "~"]
        set mini_str [lindex $parts 0]
        set maxi_str [lindex $parts 1]

        set mini_vals [split $mini_str ","]
        set maxi_vals [split $maxi_str ","]

        set mini [create_coord [lindex $mini_vals 0] [lindex $mini_vals 1] [lindex $mini_vals 2]]
        set maxi [create_coord [lindex $maxi_vals 0] [lindex $maxi_vals 1] [lindex $maxi_vals 2]]

        lappend bricks [create_brick $mini $maxi]
    }
    return $bricks
}

proc compare_bricks_by_maxz {b1 b2} {
    set z1 [dict get $b1 maxi z]
    set z2 [dict get $b2 maxi z]
    return [expr {$z1 - $z2}]
}

proc settle {bricks} {
    set indexed_bricks [list]
    for {set i 0} {$i < [llength $bricks]} {incr i} {
        set brick [lindex $bricks $i]
        dict set brick basedOn [list]
        dict set brick support [list]
        dict set brick orig_idx $i
        lappend indexed_bricks $brick
    }
    set sorted_indexed_bricks [lsort -command compare_bricks_by_maxz $indexed_bricks]

    set orig_idx_map [dict create]
    for {set i 0} {$i < [llength $sorted_indexed_bricks]} {incr i} {
        set brick [lindex $sorted_indexed_bricks $i]
        dict set orig_idx_map [dict get $brick orig_idx] $brick
    }

    for {set i 0} {$i < [llength $sorted_indexed_bricks]} {incr i} {
        set current_brick [lindex $sorted_indexed_bricks $i]
        set current_orig_idx [dict get $current_brick orig_idx]

        set supportZ 0
        set basedBricks_orig_indices [list]

        for {set j [expr {$i - 1}]} {$j >= 0} {incr j -1} {
            set settled_brick [lindex $sorted_indexed_bricks $j]

            if {[is_intersecting $current_brick $settled_brick]} {
                set settled_max_z [dict get $settled_brick maxi z]
                if {$settled_max_z > $supportZ} {
                    set supportZ $settled_max_z
                    set basedBricks_orig_indices [list [dict get $settled_brick orig_idx]]
                } elseif {$settled_max_z == $supportZ} {
                    lappend basedBricks_orig_indices [dict get $settled_brick orig_idx]
                }
            }
        }

        dict set current_brick basedOn $basedBricks_orig_indices

        set deltaZ [expr {[dict get $current_brick maxi z] - [dict get $current_brick mini z]}]
        dict set current_brick mini z [expr {$supportZ + 1}]
        dict set current_brick maxi z [expr {[dict get $current_brick mini z] + $deltaZ}]

        lset sorted_indexed_bricks $i $current_brick
        dict set orig_idx_map $current_orig_idx $current_brick
    }

    foreach brick [dict values $orig_idx_map] {
         set current_orig_idx [dict get $brick orig_idx]
         set based_orig_indices [dict get $brick basedOn]
         foreach based_orig_idx $based_orig_indices {
             set based_brick [dict get $orig_idx_map $based_orig_idx]
             dict lappend based_brick support $current_orig_idx
             dict set orig_idx_map $based_orig_idx $based_brick
         }
    }

    return [dict values $orig_idx_map]
}

proc solve {bricks} {
    set count 0
    set orig_idx_map [dict create]
    foreach brick $bricks {
        dict set orig_idx_map [dict get $brick orig_idx] $brick
    }

    foreach brick $bricks {
        set is_disintegratable true
        set supported_orig_indices [dict get $brick support]

        foreach supported_orig_idx $supported_orig_indices {
            set supported_brick [dict get $orig_idx_map $supported_orig_idx]
            set based_on_indices [dict get $supported_brick basedOn]
            if {[llength $based_on_indices] < 2} {
                set is_disintegratable false
                break
            }
        }

        if {$is_disintegratable} {
            incr count
        }
    }
    return $count
}

proc main {} {
    set filename "input.txt"
    set file [open $filename r]
    set content [read $file]
    close $file

    set lines [split [string trim $content] "\n"]
    set bricks [parseInput $lines]

    set settled_bricks [settle $bricks]

    set answer [solve $settled_bricks]
    puts $answer
}

main
