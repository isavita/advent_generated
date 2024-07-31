proc isWall {favoriteNumber x y} {
    set num [expr {$x*$x + 3*$x + 2*$x*$y + $y + $y*$y + $favoriteNumber}]
    set bits 0
    while {$num > 0} {
        set bits [expr {$bits + ($num % 2)}]
        set num [expr {$num / 2}]
    }
    return [expr {$bits % 2 != 0}]
}

proc bfs {start target favoriteNumber} {
    set visited [dict create]
    set queue [list $start]
    set steps 0

    while {[llength $queue] > 0} {
        set size [llength $queue]
        for {set i 0} {$i < $size} {incr i} {
            set point [lindex $queue $i]
            if {$point eq $target} {
                return $steps
            }
            foreach delta {{1 0} {-1 0} {0 1} {0 -1}} {
                set next [list [expr {[lindex $point 0] + [lindex $delta 0]}] [expr {[lindex $point 1] + [lindex $delta 1]}]]
                if {[lindex $next 0] >= 0 && [lindex $next 1] >= 0 && ![isWall $favoriteNumber [lindex $next 0] [lindex $next 1]] && ![dict exists $visited $next]} {
                    dict set visited $next true
                    lappend queue $next
                }
            }
        }
        set queue [lrange $queue $size end]
        incr steps
    }
    return -1
}

set infile [open "input.txt" r]
set favoriteNumber [string trim [read $infile]]
close $infile

set start {1 1}
set target {31 39}
set steps [bfs $start $target $favoriteNumber]
puts $steps