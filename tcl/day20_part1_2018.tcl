
proc move {p dir} {
    lassign $p x y
    switch -- $dir {
        N { return [list $x [expr {$y - 1}]] }
        S { return [list $x [expr {$y + 1}]] }
        E { return [list [expr {$x + 1}] $y] }
        W { return [list [expr {$x - 1}] $y] }
        default { return $p }
    }
}

proc build_map {regex} {
    set dm [dict create]
    set stack {}
    set cp {0 0}
    foreach c [split $regex ""] {
        switch -- $c {
            ( { lappend stack $cp }
            | { set cp [lindex $stack end] }
            ) { 
                set cp [lindex $stack end]
                set stack [lrange $stack 0 end-1]
            }
            default {
                set np [move $cp $c]
                if {![dict exists $dm $cp]} {
                    dict set dm $cp [dict create]
                }
                dict set dm $cp $np 1
                set cp $np
            }
        }
    }
    return $dm
}

proc find_furthest_room {dm} {
    set visited [dict create]
    set queue [list {0 0}]
    set max_doors 0
    dict set visited {0 0} 0

    while {[llength $queue] > 0} {
        set p [lindex $queue 0]
        set queue [lrange $queue 1 end]

        if {[dict exists $dm $p]} {
            set neighbors [dict keys [dict get $dm $p]]
            foreach np $neighbors {
                if {![dict exists $visited $np]} {
                    set doors [expr {[dict get $visited $p] + 1}]
                    dict set visited $np $doors
                    if {$doors > $max_doors} {
                        set max_doors $doors
                    }
                    lappend queue $np
                }
            }
        }
    }
    return $max_doors
}

proc main {} {
    set file [open "input.txt" r]
    set regex [string trim [read $file]]
    close $file
    set regex [string range $regex 1 end-1]
    set dm [build_map $regex]
    set max_doors [find_furthest_room $dm]
    puts $max_doors
}

main
