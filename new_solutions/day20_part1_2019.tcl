proc readMaze {filename} {
    set file [open $filename r]
    set maze [split [read $file] "\n"]
    close $file
    return $maze
}

proc findPortals {maze} {
    array set portals {}
    for {set y 0} {$y < [llength $maze]} {incr y} {
        set line [lindex $maze $y]
        for {set x 0} {$x < [string length $line]} {incr x} {
            set c [string index $line $x]
            if {[string is alpha $c]} {
                set label ""
                if {$x < [string length $line] - 1 && [string is alpha [string index $line [expr {$x + 1}]]]} {
                    set label [string range $line $x [expr {$x + 1}]]
                    set pos [list $x $y]
                    if {$x > 0 && [string index $line [expr {$x - 1}]] eq "."} {
                        set pos [list [expr {$x - 1}] $y]
                    } elseif {$x < [string length $line] - 2 && [string index $line [expr {$x + 2}]] eq "."} {
                        set pos [list [expr {$x + 2}] $y]
                    }
                    set portals($label) [lappend portals($label) $pos]
                    incr x
                } elseif {$y < [llength $maze] - 1 && [string is alpha [string index [lindex $maze [expr {$y + 1}]] $x]]} {
                    set label [string index $line $x][string index [lindex $maze [expr {$y + 1}]] $x]
                    set pos [list $x $y]
                    if {$y > 0 && [string index [lindex $maze [expr {$y - 1}]] $x] eq "."} {
                        set pos [list $x [expr {$y - 1}]]
                    } elseif {$y < [llength $maze] - 2 && [string index [lindex $maze [expr {$y + 2}]] $x] eq "."} {
                        set pos [list $x [expr {$y + 2}]]
                    }
                    set portals($label) [lappend portals($label) $pos]
                }
            }
        }
    }
    return [array get portals]
}

proc bfs {maze start end portals} {
    set queue [list [list $start 0]]
    array set visited {}
    set visited([join $start ","]) 1

    while {[llength $queue] > 0} {
        set current [lindex $queue 0]
        set queue [lrange $queue 1 end]
        set pos [lindex $current 0]
        set steps [lindex $current 1]

        if {$pos eq $end} {
            return $steps
        }

        foreach dir {{0 1} {1 0} {0 -1} {-1 0}} {
            set next [list [expr {[lindex $pos 0] + [lindex $dir 0]}] [expr {[lindex $pos 1] + [lindex $dir 1]}]]
            if {![info exists visited([join $next ","])] && [string index [lindex $maze [lindex $next 1]] [lindex $next 0]] eq "."} {
                lappend queue [list $next [expr {$steps + 1}]]
                set visited([join $next ","]) 1
            }
        }

        foreach {label positions} $portals {
            if {[lsearch -exact $positions $pos] >= 0} {
                foreach p $positions {
                    if {$p ne $pos && ![info exists visited([join $p ","])]} {
                        lappend queue [list $p [expr {$steps + 1}]]
                        set visited([join $p ","]) 1
                    }
                }
            }
        }
    }
    return -1
}

proc main {} {
    set maze [readMaze "input.txt"]
    set portals [findPortals $maze]
    set start [lindex [dict get $portals AA] 0]
    set end [lindex [dict get $portals ZZ] 0]
    set steps [bfs $maze $start $end $portals]
    puts $steps
}

main