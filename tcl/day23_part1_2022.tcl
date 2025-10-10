
#!/usr/bin/env tclsh

# Read the grid
set f [open input.txt r]
set y 0
set elves [dict create]
while {[gets $f line] >= 0} {
    set x 0
    foreach c [split $line ""] {
        if {$c eq "#"} { dict set elves $x,$y 1 }
        incr x
    }
    incr y
}
close $f

# Directions and checks
set moves { {0 -1} {0 1} {-1 0} {1 0} }
set checks {
    { {-1 -1} {0 -1} {1 -1} }
    { {-1 1} {0 1} {1 1} }
    { {-1 -1} {-1 0} {-1 1} }
    { {1 -1} {1 0} {1 1} }
}
set adj { {-1 -1} {0 -1} {1 -1} {-1 0} {1 0} {-1 1} {0 1} {1 1} }

set dir_order {0 1 2 3}

for {set round 0} {$round < 10} {incr round} {
    # Proposal phase
    set proposals [dict create]
    set propose_from [dict create]
    dict for {pos _} $elves {
        lassign [split $pos ,] x y
        set has_neighbor 0
        foreach d $adj {
            set nx [expr {$x + [lindex $d 0]}]
            set ny [expr {$y + [lindex $d 1]}]
            if {[dict exists $elves $nx,$ny]} { set has_neighbor 1; break }
        }
        if {!$has_neighbor} continue
        for {set k 0} {$k < 4} {incr k} {
            set dir_idx [lindex $dir_order $k]
            set ok 1
            foreach check [lindex $checks $dir_idx] {
                set cx [expr {$x + [lindex $check 0]}]
                set cy [expr {$y + [lindex $check 1]}]
                if {[dict exists $elves $cx,$cy]} { set ok 0; break }
            }
            if {$ok} {
                set move [lindex $moves $dir_idx]
                set nx [expr {$x + [lindex $move 0]}]
                set ny [expr {$y + [lindex $move 1]}]
                set dest $nx,$ny
                if {[dict exists $proposals $dest]} {
                    dict incr proposals $dest
                } else {
                    dict set proposals $dest 1
                    dict set propose_from $dest $x,$y
                }
                break
            }
        }
    }

    # Execution phase
    dict for {dest cnt} $proposals {
        if {$cnt == 1} {
            set from [dict get $propose_from $dest]
            dict unset elves $from
            dict set elves $dest 1
        }
    }

    # Rotate direction order
    set dir_order [linsert [lrange $dir_order 1 end] end [lindex $dir_order 0]]
}

# Compute bounding box and empty tiles
set xs {}
set ys {}
dict for {pos _} $elves {
    lassign [split $pos ,] x y
    lappend xs $x
    lappend ys $y
}
set minx [tcl::mathfunc::min {*}$xs]
set maxx [tcl::mathfunc::max {*}$xs]
set miny [tcl::mathfunc::min {*}$ys]
set maxy [tcl::mathfunc::max {*}$ys]

set width  [expr {$maxx - $minx + 1}]
set height [expr {$maxy - $miny + 1}]
set area   [expr {$width * $height}]
set elves_count [dict size $elves]
puts [expr {$area - $elves_count}]
