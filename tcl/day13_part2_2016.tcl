set favoriteNumber 1362

proc isWall {x y} {
    global favoriteNumber
    set num [expr {$x*$x + 3*$x + 2*$x*$y + $y + $y*$y + $favoriteNumber}]
    set bits 0
    while {$num > 0} {
        set bits [expr {$bits + ($num % 2)}]
        set num [expr {$num / 2}]
    }
    return [expr {$bits % 2 != 0}]
}

proc bfsMaxSteps {start maxSteps} {
    set visited [dict create]
    set queue [list $start]
    dict set visited $start 1
    set steps 0

    while {[llength $queue] > 0 && $steps < $maxSteps} {
        set size [llength $queue]
        for {set i 0} {$i < $size} {incr i} {
            set point [lindex $queue $i]
            set x [lindex $point 0]
            set y [lindex $point 1]

            foreach delta {{1 0} {-1 0} {0 1} {0 -1}} {
                set next [list [expr {$x + [lindex $delta 0]}] [expr {$y + [lindex $delta 1]}]]
                if {[lindex $next 0] >= 0 && [lindex $next 1] >= 0 && ![isWall [lindex $next 0] [lindex $next 1]] && ![dict exists $visited $next]} {
                    dict set visited $next 1
                    lappend queue $next
                }
            }
        }
        set queue [lrange $queue $size end]
        incr steps
    }

    return [dict size $visited]
}

set start {1 1}
set reachableLocations [bfsMaxSteps $start 50]
puts $reachableLocations