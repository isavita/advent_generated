
#!/usr/bin/env tclsh

proc readInput {} {
    set fp [open "input.txt" r]
    set data [read $fp]
    close $fp
    return [string trim $data]
}

proc abs {x} {
    return [expr {abs($x)}]
}

proc sign {n} {
    if {$n == 0} {return 0}
    return [expr {$n > 0 ? 1 : -1}]
}

proc next {head tail} {
    lassign $head hx hy
    lassign $tail tx ty

    if {[abs [expr {$hx - $tx}]] <= 1 && [abs [expr {$hy - $ty}]] <= 1} {
        return $tail
    }

    return [list \
        [expr {$tx + [sign [expr {$hx - $tx}]]}] \
        [expr {$ty + [sign [expr {$hy - $ty}]]}] \
    ]
}

proc visited {input ropelen} {
    set rope [lrepeat $ropelen {0 0}]
    set visited [dict create]

    foreach line [split $input "\n"] {
        scan $line "%s %d" dir steps
        
        switch $dir {
            "U" - "N" { set delta {0 1} }
            "D" - "S" { set delta {0 -1} }
            "R" - "E" { set delta {1 0} }
            "L" - "W" { set delta {-1 0} }
        }

        for {set i 0} {$i < $steps} {incr i} {
            # Move head
            lassign [lindex $rope 0] hx hy
            lassign $delta dx dy
            set rope [lreplace $rope 0 0 [list [expr {$hx + $dx}] [expr {$hy + $dy}]]]

            # Move rest of the rope
            for {set j 1} {$j < $ropelen} {incr j} {
                set rope [lreplace $rope $j $j [next [lindex $rope [expr {$j-1}]] [lindex $rope $j]]]
            }

            # Mark tail position
            dict set visited [lindex $rope [expr {$ropelen-1}]] 1
        }
    }

    return [dict size $visited]
}

set input [readInput]
puts [visited $input 10]
