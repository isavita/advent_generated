proc read_input {filename} {
    set f [open $filename r]
    set lines [split [read $f] "\n"]
    close $f
    return [lrange $lines 0 end-1]
}

proc find_portals {grid} {
    set portals [dict create]
    set height [llength $grid]
    set width 0
    foreach row $grid {
        set len [string length $row]
        if {$len > $width} {set width $len}
    }
    
    for {set y 0} {$y < $height} {incr y} {
        set row [lindex $grid $y]
        set row [format "%-${width}s" $row]
        for {set x 0} {$x < $width} {incr x} {
            if {$x < $width - 1} {
                set c1 [string index $row $x]
                set c2 [string index $row [expr {$x + 1}]]
                if {[string is upper $c1] && [string is upper $c2]} {
                    set label $c1$c2
                    set pos {}
                    if {$x - 1 >= 0 && [string index $row [expr {$x - 1}]] eq "."} {
                        set pos [list [expr {$x - 1}] $y]
                    } elseif {$x + 2 < $width && [string index $row [expr {$x + 2}]] eq "."} {
                        set pos [list [expr {$x + 2}] $y]
                    }
                    if {$pos ne {}} {
                        dict lappend portals $label $pos
                    }
                }
            }
            if {$y < $height - 1} {
                set row2 [lindex $grid [expr {$y + 1}]]
                set row2 [format "%-${width}s" $row2]
                set c1 [string index $row $x]
                set c2 [string index $row2 $x]
                if {[string is upper $c1] && [string is upper $c2]} {
                    set label $c1$c2
                    set pos {}
                    if {$y - 1 >= 0 && [string index [lindex $grid [expr {$y - 1}]] $x] eq "."} {
                        set pos [list $x [expr {$y - 1}]]
                    } elseif {$y + 2 < $height && [string index [lindex $grid [expr {$y + 2}]] $x] eq "."} {
                        set pos [list $x [expr {$y + 2}]]
                    }
                    if {$pos ne {}} {
                        dict lappend portals $label $pos
                    }
                }
            }
        }
    }
    return [list $portals $width $height]
}

proc is_outer {pos width height} {
    lassign $pos x y
    return [expr {$x <= 2 || $y <= 2 || $x >= $width - 3 || $y >= $height - 3}]
}

proc build_portal_mapping {portals width height} {
    set portal_map [dict create]
    set start {}
    set end {}
    dict for {label positions} $portals {
        if {$label eq "AA"} {
            set start [lindex $positions 0]
        } elseif {$label eq "ZZ"} {
            set end [lindex $positions 0]
        } elseif {[llength $positions] == 2} {
            lassign $positions a b
            set a_outer [is_outer $a $width $height]
            set b_outer [is_outer $b $width $height]
            dict set portal_map $a [list $b $a_outer]
            dict set portal_map $b [list $a $b_outer]
        }
    }
    return [list $start $end $portal_map]
}

proc bfs_recursive {grid start end portal_map width height} {
    set queue [list [list {*}$start 0 0]]
    set visited [dict create]
    dict set visited [list {*}$start 0] 1
    set directions {0 1 0 -1 1 0 -1 0}
    
    while {[llength $queue] > 0} {
        lassign [lindex $queue 0] x y level steps
        set queue [lrange $queue 1 end]
        
        if {$x == [lindex $end 0] && $y == [lindex $end 1] && $level == 0} {
            return $steps
        }
        
        foreach {dx dy} $directions {
            set nx [expr {$x + $dx}]
            set ny [expr {$y + $dy}]
            if {$ny < 0 || $ny >= [llength $grid] || $nx < 0 || $nx >= [string length [lindex $grid $ny]]} {
                continue
            }
            if {[string index [lindex $grid $ny] $nx] ne "."} {
                continue
            }
            set state [list $nx $ny $level]
            if {![dict exists $visited $state]} {
                dict set visited $state 1
                lappend queue [list $nx $ny $level [expr {$steps + 1}]]
            }
        }
        
        set pos [list $x $y]
        if {[dict exists $portal_map $pos]} {
            lassign [dict get $portal_map $pos] target outer
            set new_level [expr {$outer ? $level - 1 : $level + 1}]
            if {$new_level >= 0} {
                lassign $target tx ty
                set state [list $tx $ty $new_level]
                if {![dict exists $visited $state]} {
                    dict set visited $state 1
                    lappend queue [list $tx $ty $new_level [expr {$steps + 1}]]
                }
            }
        }
    }
    return -1
}

proc main {} {
    set grid [read_input "input.txt"]
    lassign [find_portals $grid] portals width height
    lassign [build_portal_mapping $portals $width $height] start end portal_map
    set result [bfs_recursive $grid $start $end $portal_map $width $height]
    puts $result
}

main