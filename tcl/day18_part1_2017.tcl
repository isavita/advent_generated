
proc main {} {
    set f [open "input.txt" r]
    set instructions [split [read $f] \n]
    close $f

    set registers [dict create]
    set current_instruction 0
    set last_sound 0

    while {0 <= $current_instruction && $current_instruction < [llength $instructions]} {
        set instruction [lindex $instructions $current_instruction]
        set instruction_list [split $instruction]
        set command [lindex $instruction_list 0]
        set register [lindex $instruction_list 1]

        if {![dict exists $registers $register]} {
            dict set registers $register 0
        }

        if {[llength $instruction_list] == 3} {
            set value [lindex $instruction_list 2]
            if {[string is alpha $value]} {
                set value [dict get $registers $value]
            } else {
                set value [expr {$value}]
            }
        }

        switch -- $command {
            snd {
                set last_sound [dict get $registers $register]
            }
            set {
                dict set registers $register $value
            }
            add {
                dict set registers $register [expr {[dict get $registers $register] + $value}]
            }
            mul {
                dict set registers $register [expr {[dict get $registers $register] * $value}]
            }
            mod {
                dict set registers $register [expr {[dict get $registers $register] % $value}]
            }
            rcv {
                if {[dict get $registers $register] != 0} {
                    puts $last_sound
                    break
                }
            }
            jgz {
                if {[dict get $registers $register] > 0} {
                    incr current_instruction $value
                    continue
                }
            }
        }
        incr current_instruction
    }
}

main
