set input [open "input.txt" r]
set data [read $input]
close $input

set grid [dict create]
set parts [list]
set curr {}

set y 0
foreach line [split $data "\n"] {
    if {$curr ne ""} {
        lappend parts $curr
        set curr {}
    }
    set x 0
    foreach c [split $line ""] {
        dict set grid [list $x $y] $c
        if {[string is digit $c]} {
            if {$curr eq ""} {
                set curr [list $x $x $y $c]
            } else {
                lset curr 1 $x
                lset curr 3 [expr {[lindex $curr 3] * 10 + $c}]
            }
        } elseif {$curr ne ""} {
            lappend parts $curr
            set curr {}
        }
        incr x
    }
    incr y
}

set parts_grid [dict create]
set i 0
foreach p $parts {
    for {set x [lindex $p 0]} {$x <= [lindex $p 1]} {incr x} {
        dict set parts_grid [list $x [lindex $p 2]] $i
    }
    incr i
}

set sum 0
set neighbors {
    {0 1} {0 -1} {1 0} {-1 0}
    {-1 -1} {-1 1} {1 -1} {1 1}
}

dict for {pos c} $grid {
    if {$c eq "*"} {
        set neighbor_parts [dict create]
        foreach neighbor $neighbors {
            set neighbor_pos [list \
                [expr {[lindex $pos 0] + [lindex $neighbor 0]}] \
                [expr {[lindex $pos 1] + [lindex $neighbor 1]}]]
            if {[dict exists $parts_grid $neighbor_pos]} {
                dict set neighbor_parts [dict get $parts_grid $neighbor_pos] 1
            }
        }
        if {[dict size $neighbor_parts] == 2} {
            set prod 1
            dict for {idx _} $neighbor_parts {
                set prod [expr {$prod * [lindex [lindex $parts $idx] 3]}]
            }
            incr sum $prod
        }
    }
}

puts $sum