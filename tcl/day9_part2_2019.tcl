
proc intcode {program input} {
    array set memory {}
    for {set i 0} {$i < [llength $program]} {incr i} {
        set memory($i) [lindex $program $i]
    }

    set pc 0
    set relative_base 0
    set output ""
    set halt 0

    while {!$halt} {
        set opcode [expr {$memory($pc) % 100}]
        set mode1 [expr {($memory($pc) / 100) % 10}]
        set mode2 [expr {($memory($pc) / 1000) % 10}]
        set mode3 [expr {($memory($pc) / 10000) % 10}]

        proc get_param {mode offset} {
            upvar memory memory
            upvar pc pc
            upvar relative_base relative_base

            set val $memory([expr {$pc + $offset}])
            switch $mode {
                0 {
                    if {![info exists memory($val)]} {set memory($val) 0}
                    return $memory($val)
                }
                1 {
                    return $val
                }
                2 {
                    set addr [expr {$relative_base + $val}]
                    if {![info exists memory($addr)]} {set memory($addr) 0}
                    return $memory($addr)
                }
            }
        }

        proc set_param {mode offset value} {
            upvar memory memory
            upvar pc pc
            upvar relative_base relative_base

            set val $memory([expr {$pc + $offset}])
            switch $mode {
                0 {
                    set memory($val) $value
                }
                2 {
                    set addr [expr {$relative_base + $val}]
                    set memory($addr) $value
                }
            }
        }
        
        switch $opcode {
            1 { ;# Add
                set param1 [get_param $mode1 1]
                set param2 [get_param $mode2 2]
                set_param $mode3 3 [expr {$param1 + $param2}]
                incr pc 4
            }
            2 { ;# Multiply
                set param1 [get_param $mode1 1]
                set param2 [get_param $mode2 2]
                set_param $mode3 3 [expr {$param1 * $param2}]
                incr pc 4
            }
            3 { ;# Input
                set_param $mode1 1 $input
                incr pc 2
            }
            4 { ;# Output
                set param1 [get_param $mode1 1]
                append output $param1 " "
                incr pc 2
            }
            5 { ;# Jump-if-true
                set param1 [get_param $mode1 1]
                set param2 [get_param $mode2 2]
                if {$param1 != 0} {
                    set pc $param2
                } else {
                    incr pc 3
                }
            }
            6 { ;# Jump-if-false
                set param1 [get_param $mode1 1]
                set param2 [get_param $mode2 2]
                if {$param1 == 0} {
                    set pc $param2
                } else {
                    incr pc 3
                }
            }
            7 { ;# Less than
                set param1 [get_param $mode1 1]
                set param2 [get_param $mode2 2]
                if {$param1 < $param2} {
                    set_param $mode3 3 1
                } else {
                    set_param $mode3 3 0
                }
                incr pc 4
            }
            8 { ;# Equals
                set param1 [get_param $mode1 1]
                set param2 [get_param $mode2 2]
                if {$param1 == $param2} {
                    set_param $mode3 3 1
                } else {
                    set_param $mode3 3 0
                }
                incr pc 4
            }
            9 { ;# Adjust relative base
                set param1 [get_param $mode1 1]
                incr relative_base $param1
                incr pc 2
            }
            99 { ;# Halt
                set halt 1
            }
            default {
                puts "Error: Unknown opcode $opcode at position $pc"
                return
            }
        }
    }
    return $output
}

# Main program
if {[file exists "input.txt"]} {
    set inputFile "input.txt"
} else {
    puts "Error: input.txt not found."
    exit 1
}

set fp [open $inputFile r]
set program [split [read $fp] ,]
close $fp

# Part 1
set output1 [intcode $program 1]
puts "Part 1: $output1"

# Part 2
set output2 [intcode $program 2]
puts "Part 2: $output2"
