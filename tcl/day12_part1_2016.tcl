proc main {} {
    array set registers {a 0 b 0 c 0 d 0}
    set instructions [split [read [open "input.txt"]] "\n"]
    set pc 0

    while {$pc < [llength $instructions]} {
        set parts [split [lindex $instructions $pc]]
        switch [lindex $parts 0] {
            "cpy" {
                set x [lindex $parts 1]
                set y [lindex $parts 2]
                if {[string is integer $x]} {
                    set registers($y) $x
                } else {
                    set registers($y) [set registers($x)]
                }
                incr pc
            }
            "inc" {
                set x [lindex $parts 1]
                incr registers($x)
                incr pc
            }
            "dec" {
                set x [lindex $parts 1]
                incr registers($x) -1
                incr pc
            }
            "jnz" {
                set x [lindex $parts 1]
                set y [lindex $parts 2]
                if {[string is integer $x] && $x != 0} {
                    incr pc $y
                } elseif {[set registers($x)] != 0} {
                    incr pc $y
                } else {
                    incr pc
                }
            }
            default {
                incr pc
            }
        }
    }
    puts $registers(a)
}

main