proc execute_code {commands c_init} {
    set registers(a) 0
    set registers(b) 0
    set registers(c) $c_init
    set registers(d) 0
    set pc 0

    set n [llength $commands]
    while {$pc < $n} {
        set line [lindex $commands $pc]
        set parts [split $line]
        switch -- [lindex $parts 0] {
            cpy {
                set x [lindex $parts 1]
                set y [lindex $parts 2]
                if {[string is integer $x]} {
                    set registers($y) $x
                } else {
                    set registers($y) [set registers($x)]
                }
                incr pc
            }
            inc {
                set x [lindex $parts 1]
                incr registers($x)
                incr pc
            }
            dec {
                set x [lindex $parts 1]
                incr registers($x) -1
                incr pc
            }
            jnz {
                set x [lindex $parts 1]
                set y [lindex $parts 2]
                if {[string is integer $x] && $x != 0} {
                    set pc [expr {$pc + [string trim $y]}]
                } elseif {![string is integer $x]} {
                    if {[set registers($x)] != 0} {
                        set pc [expr {$pc + [string trim $y]}]
                    } else {
                        incr pc
                    }
                } else {
                    incr pc
                }
            }
            default {
                incr pc
            }
        }
    }
    return $registers(a)
}

set filename "input.txt"
set fileId [open $filename r]
set commands [split [read $fileId] \n]
close $fileId

# Part One: Initialize c to 0
set result1 [execute_code $commands 0]
puts "Part One: Register A = $result1"

# Part Two: Initialize c to 1
set result2 [execute_code $commands 1]
puts "Part Two: Register A = $result2"