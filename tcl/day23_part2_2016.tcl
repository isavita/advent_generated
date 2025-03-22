
proc is_register {x} {
    return [expr {[string is alpha $x] && [string length $x] == 1 && [string match {[abcd]} $x]}]
}

proc get_value {x registers} {
    if {[is_register $x]} {
        return [dict get $registers $x]
    } else {
        return [expr {$x}]
    }
}

proc execute_program {instructions registers} {
    set i 0
    set len_instructions [llength $instructions]

    while {$i < $len_instructions} {
        if {$i + 5 < $len_instructions} {
            set pattern [lrange $instructions $i [expr {$i + 5}]]
            set pattern0 [lindex $pattern 0]
            set pattern1 [lindex $pattern 1]
            set pattern2 [lindex $pattern 2]
            set pattern3 [lindex $pattern 3]
            set pattern4 [lindex $pattern 4]
            set pattern5 [lindex $pattern 5]

            if {([string match {cpy *} $pattern0]) &&
                ([string match {inc *} $pattern1]) &&
                ([string match {dec *} $pattern2]) &&
                ([string match {jnz *} $pattern3]) &&
                ([string match {dec *} $pattern4]) &&
                ([string match {jnz *} $pattern5])} {

                set cpy_x [lindex [split $pattern0] 1]
                set cpy_y [lindex [split $pattern0] 2]
                set inc_a [lindex [split $pattern1] 1]
                set dec_c [lindex [split $pattern2] 1]
                set jnz_c [lindex [split $pattern3] 1]
                set jnz_c_offset [lindex [split $pattern3] 2]
                set dec_d [lindex [split $pattern4] 1]
                set jnz_d [lindex [split $pattern5] 1]
                set jnz_d_offset [lindex [split $pattern5] 2]

                if {($inc_a eq "a") && ($dec_c eq $cpy_y) && ($jnz_c eq $cpy_y) && ($jnz_c_offset eq "-2") &&
                    ($dec_d eq "d") && ($jnz_d eq "d") && ($jnz_d_offset eq "-5")} {

                    dict set registers a [expr {[dict get $registers a] + [get_value $cpy_x $registers] * [dict get $registers d]}]
                    dict set registers $cpy_y 0
                    dict set registers d 0
                    incr i 6
                    continue
                }
            }
        }

        set parts [split [lindex $instructions $i]]
        set cmd [lindex $parts 0]

        if {$cmd eq "tgl"} {
            set x [get_value [lindex $parts 1] $registers]
            set target_idx [expr {$i + $x}]
            if {$target_idx >= 0 && $target_idx < $len_instructions} {
                set target_parts [split [lindex $instructions $target_idx]]
                set target_cmd [lindex $target_parts 0]
                set num_parts [llength $target_parts]

                if {$num_parts == 2} {
                    if {$target_cmd eq "inc"} {
                        lset target_parts 0 "dec"
                    } else {
                        lset target_parts 0 "inc"
                    }
                } elseif {$num_parts == 3} {
                    if {$target_cmd eq "jnz"} {
                        lset target_parts 0 "cpy"
                    } else {
                        lset target_parts 0 "jnz"
                    }
                }
                lset instructions $target_idx [join $target_parts " "]
            }
            incr i
            continue
        }

        switch -- $cmd {
            "cpy" {
                set x [lindex $parts 1]
                set y [lindex $parts 2]
                if {[is_register $y]} {
                    dict set registers $y [get_value $x $registers]
                }
                incr i
            }
            "inc" {
                set x [lindex $parts 1]
                if {[is_register $x]} {
                    dict set registers $x [expr {[dict get $registers $x] + 1}]
                }
                incr i
            }
            "dec" {
                set x [lindex $parts 1]
                if {[is_register $x]} {
                    dict set registers $x [expr {[dict get $registers $x] - 1}]
                }
                incr i
            }
            "jnz" {
                set x [lindex $parts 1]
                set y [lindex $parts 2]
                if {[get_value $x $registers] != 0} {
                    incr i [get_value $y $registers]
                } else {
                    incr i
                }
            }
            default {
                incr i
            }
        }
    }
    return $registers
}

proc main {} {
    set file [open "input.txt" r]
    set instructions [split [read $file] "\n"]
    close $file
    
    set registers [dict create a 12 b 0 c 0 d 0]
    set final_registers [execute_program $instructions $registers]
    puts [dict get $final_registers a]
}

main
