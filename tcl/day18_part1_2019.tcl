
#!/usr/bin/env tclsh

proc solve {} {
    set file [open "input.txt" r]
    set grid [list]
    set keyMap [dict create]
    set keyCounter 0
    set start {}

    set y 0
    while {[gets $file line] != -1} {
        lappend grid $line
        set lineChars [split $line ""]
        for {set x 0} {$x < [string length $line]} {incr x} {
            set char [lindex $lineChars $x]
            if {$char eq "@"} {
                set start [list $x $y]
            } elseif {[regexp {[a-z]} $char]} {
                dict set keyMap $char $keyCounter
                incr keyCounter
            }
        }
        incr y
    }
    close $file

    puts [findShortestPath $grid $start $keyMap]
}

proc findShortestPath {grid start keyMap} {
    set dirs {{0 -1} {-1 0} {0 1} {1 0}}
    set visited [dict create]
    set queue [list [list $start 0]]
    set steps 0
    set maxKeys [expr {(1 << [dict size $keyMap]) - 1}]

    while {[llength $queue] > 0} {
        set size [llength $queue]
        for {set i 0} {$i < $size} {incr i} {
            set current [lindex $queue 0]
            set queue [lrange $queue 1 end]

            lassign $current pos keys
            lassign $pos x y

            if {$keys == $maxKeys} {
                return $steps
            }

            foreach dir $dirs {
                lassign $dir dx dy
                set nx [expr {$x + $dx}]
                set ny [expr {$y + $dy}]

                if {$nx >= 0 && $nx < [string length [lindex $grid 0]] && 
                    $ny >= 0 && $ny < [llength $grid]} {
                    set char [string index [lindex $grid $ny] $nx]

                    if {$char ne "#" && 
                        !([regexp {[A-Z]} $char] && 
                          ($keys & (1 << [dict get $keyMap [string tolower $char]])) == 0)} {
                        set newKeys $keys
                        if {[regexp {[a-z]} $char]} {
                            set newKeys [expr {$keys | (1 << [dict get $keyMap $char])}]
                        }

                        set newState [list [list $nx $ny] $newKeys]
                        set stateKey [join $newState ,]

                        if {![dict exists $visited $stateKey]} {
                            dict set visited $stateKey 1
                            lappend queue $newState
                        }
                    }
                }
            }
        }
        incr steps
    }

    return -1
}

solve
