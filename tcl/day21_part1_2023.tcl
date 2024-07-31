proc read_file {filename} {
    set file [open $filename r]
    set content [read $file]
    close $file
    return [split $content "\n"]
}

proc coord_add {c1 c2} {
    return [list [expr {[lindex $c1 0] + [lindex $c2 0]}] [expr {[lindex $c1 1] + [lindex $c2 1]}]]
}

proc multiply_by_scalar {c s} {
    return [list [expr {[lindex $c 0] * $s}] [expr {[lindex $c 1] * $s}]]
}

proc is_in_bounds {width height coord} {
    return [expr {[lindex $coord 0] >= 0 && [lindex $coord 0] < $width && [lindex $coord 1] >= 0 && [lindex $coord 1] < $height}]
}

proc parse_input {input} {
    set grid [list]
    set width [string length [lindex $input 0]]
    set height [llength $input]
    foreach line $input {
        set row [list]
        foreach char [split $line ""] {
            lappend row $char
        }
        lappend grid $row
    }
    return [list $grid $width $height]
}

proc find_start {grid} {
    set y 0
    foreach row $grid {
        set x 0
        foreach char $row {
            if {$char eq "S"} {
                return [list $x $y]
            }
            incr x
        }
        incr y
    }
    error "No start found."
}

proc neighbors4 {grid coord} {
    set directions {{0 -1} {0 1} {-1 0} {1 0}}
    set neighbors {}
    foreach dir $directions {
        set neighbor [coord_add $coord $dir]
        if {[is_in_bounds [llength $grid] [llength [lindex $grid 0]] $neighbor] && !([lindex $grid [lindex $neighbor 1] [lindex $neighbor 0]] eq "#")} {
            lappend neighbors $neighbor
        }
    }
    return $neighbors
}

proc breadth_first_search {grid start} {
    set frontier [list $start]
    set reached [list $start]
    set distances [dict create]
    dict set distances $start 0
    while {[llength $frontier] > 0} {
        set current [lindex $frontier 0]
        set frontier [lrange $frontier 1 end]
        foreach next [neighbors4 $grid $current] {
            if {[lsearch -exact $reached $next] == -1} {
                lappend frontier $next
                lappend reached $next
                dict set distances $next [expr {[dict get $distances $current] + 1}]
            }
        }
    }
    return $distances
}

proc solve {input num_steps} {
    set parsed [parse_input $input]
    set grid [lindex $parsed 0]
    set width [lindex $parsed 1]
    set height [lindex $parsed 2]
    set start [find_start $grid]
    set distances [breadth_first_search $grid $start]
    set cnt 0
    foreach {coord dist} [dict get $distances] {
        if {$dist <= $num_steps && $dist % 2 == 0} {
            incr cnt
        }
    }
    return $cnt
}

set input [read_file "input.txt"]
puts [solve $input 64]