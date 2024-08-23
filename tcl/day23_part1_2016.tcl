proc toggle {instruction} {
    set parts [split $instruction]
    set cmd [lindex $parts 0]
    if {[llength $parts] == 2} {
        if {$cmd eq "inc"} {return "dec [lindex $parts 1]"}
        return "inc [lindex $parts 1]"
    } elseif {[llength $parts] == 3} {
        if {$cmd eq "jnz"} {return "cpy [lindex $parts 1] [lindex $parts 2]"}
        return "jnz [lindex $parts 1] [lindex $parts 2]"
    }
    return $instruction
}

proc execute {instructions} {
    array set registers {a 7 b 0 c 0 d 0}
    set pc 0
    set len [llength $instructions]

    while {$pc < $len} {
        set parts [split [lindex $instructions $pc]]
        set cmd [lindex $parts 0]

        switch -- $cmd {
            cpy {
                set val [lindex $parts 1]
                if {[string is integer $val]} {
                    set registers([lindex $parts 2]) $val
                } else {
                    set registers([lindex $parts 2]) $registers($val)
                }
                incr pc
            }
            inc {
                incr registers([lindex $parts 1])
                incr pc
            }
            dec {
                incr registers([lindex $parts 1]) -1
                incr pc
            }
            jnz {
                set val [lindex $parts 1]
                if {! [string is integer $val]} {
                    set val $registers($val)
                }
                set offset [lindex $parts 2]
                if {! [string is integer $offset]} {
                    set offset $registers($offset)
                }
                if {$val != 0} {
                    set pc [expr {$pc + $offset}]
                } else {
                    incr pc
                }
            }
            tgl {
                set val [lindex $parts 1]
                if {! [string is integer $val]} {
                    set val $registers($val)
                }
                set target [expr {$pc + $val}]
                if {$target >= 0 && $target < $len} {
                    set instructions [lreplace $instructions $target $target [toggle [lindex $instructions $target]]]
                }
                incr pc
            }
            default {
                incr pc
            }
        }
    }
    return $registers(a)
}

set file [open "input.txt" r]
set instructions [split [read $file] "\n"]
close $file

puts [execute $instructions]