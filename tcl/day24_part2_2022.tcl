
proc read_input {file_path} {
    set walls {}
    set blizzards {}
    set y 0
    set fd [open $file_path r]
    while {[gets $fd line] != -1} {
        set line [string trim $line]
        set x 0
        foreach char [split $line ""] {
            if {$char == "#"} {
                lappend walls [list $x $y]
            } elseif {$char in {> < ^ v}} {
                lappend blizzards [list $x $y $char]
            }
            incr x
        }
        set width $x
        incr y
    }
    close $fd
    set height $y
    return [list $walls $blizzards $height $width]
}

proc find_start_end {walls height width} {
    for {set x 0} {$x < $width} {incr x} {
        if {[lsearch -exact $walls [list $x 0]] == -1} {
            set start [list $x 0]
            break
        }
    }
    for {set x 0} {$x < $width} {incr x} {
        if {[lsearch -exact $walls [list $x [expr {$height - 1}]]] == -1} {
            set end [list $x [expr {$height - 1}]]
            break
        }
    }
    return [list $start $end]
}

proc gcd {a b} {
    while {$b != 0} {
        set t $b
        set b [expr {$a % $b}]
        set a $t
    }
    return $a
}

proc lcm {a b} {
    return [expr {abs($a * $b) / [gcd $a $b]}]
}

proc compute_period {width height} {
    return [lcm $width $height]
}

proc precompute_blizzards {blizzards width height period} {
    set blizzard_positions {}
    for {set t 0} {$t < $period} {incr t} {
        set blizz_set {}
        foreach b $blizzards {
            lassign $b x y dir
            if {$dir == ">"} {
                set new_x [expr {1 + (($x - 1 + $t) % ($width - 2))}]
                set new_y $y
            } elseif {$dir == "<"} {
                set new_x [expr {1 + (($x - 1 - $t) % ($width - 2))}]
                set new_y $y
            } elseif {$dir == "v"} {
                set new_x $x
                set new_y [expr {1 + (($y - 1 + $t) % ($height - 2))}]
            } elseif {$dir == "^"} {
                set new_x $x
                set new_y [expr {1 + (($y - 1 - $t) % ($height - 2))}]
            }
            lappend blizz_set [list $new_x $new_y]
        }
        lappend blizzard_positions $blizz_set
    }
    return $blizzard_positions
}

proc bfs {start end walls blizzard_positions period width height start_time} {
    set queue [list [list {*}$start $start_time]]
    set visited($start,[expr {$start_time % $period}]) 1
    set directions {0 0 1 0 -1 0 0 1 0 -1}
    
    while {[llength $queue] > 0} {
        lassign [lindex $queue 0] x y t
        set queue [lrange $queue 1 end]
        
        if {[list $x $y] == $end} {
            return $t
        }
        
        set next_t [expr {$t + 1}]
        set blizzards_next [lindex $blizzard_positions [expr {$next_t % $period}]]
        
        foreach {dx dy} $directions {
            set nx [expr {$x + $dx}]
            set ny [expr {$y + $dy}]
            
            if {[list $nx $ny] == $end} {
                return $next_t
            }
            
            if {[list $nx $ny] == $start} {
                set state [list $nx $ny [expr {$next_t % $period}]]
                if {![info exists visited($state)] && [lsearch -exact $blizzards_next [list $nx $ny]] == -1} {
                    set visited($state) 1
                    lappend queue [list $nx $ny $next_t]
                }
                continue
            }
            
            if {$nx >= 1 && $nx < $width - 1 && $ny >= 1 && $ny < $height - 1} {
                if {[lsearch -exact $walls [list $nx $ny]] != -1 || [lsearch -exact $blizzards_next [list $nx $ny]] != -1} {
                    continue
                }
                set state [list $nx $ny [expr {$next_t % $period}]]
                if {![info exists visited($state)]} {
                    set visited($state) 1
                    lappend queue [list $nx $ny $next_t]
                }
            }
        }
    }
    return -1
}

proc main {} {
    lassign [read_input "input.txt"] walls blizzards height width
    lassign [find_start_end $walls $height $width] start end
    set period [compute_period [expr {$width - 2}] [expr {$height - 2}]]
    set blizzard_positions [precompute_blizzards $blizzards $width $height $period]
    
    set time1 [bfs $start $end $walls $blizzard_positions $period $width $height 0]
    if {$time1 == -1} {
        puts "No path found for trip 1."
        return
    }
    
    set time2 [bfs $end $start $walls $blizzard_positions $period $width $height $time1]
    if {$time2 == -1} {
        puts "No path found for trip 2."
        return
    }
    
    set time3 [bfs $start $end $walls $blizzard_positions $period $width $height $time2]
    if {$time3 == -1} {
        puts "No path found for trip 3."
        return
    }
    
    puts $time3
}

main
