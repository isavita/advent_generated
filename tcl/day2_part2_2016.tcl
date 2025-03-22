
proc get_code {instructions keypad} {
    set code ""
    set x 1
    set y 1
    foreach instruction $instructions {
        foreach move [split $instruction ""] {
            switch -- $move {
                U {if {$x > 0 && [lindex [lindex $keypad $x-1] $y] ne 0} {incr x -1}}
                D {if {$x < [expr {[llength $keypad]-1}] && [lindex [lindex $keypad $x+1] $y] ne 0} {incr x}}
                L {if {$y > 0 && [lindex [lindex $keypad $x] $y-1] ne 0} {incr y -1}}
                R {if {$y < [expr {[llength [lindex $keypad 0]]-1}] && [lindex [lindex $keypad $x] $y+1] ne 0} {incr y}}
            }
        }
        append code [lindex [lindex $keypad $x] $y]
    }
    return $code
}

set input_file [open "input.txt" r]
set instructions [split [read $input_file] "\n"]
close $input_file

set keypad1 {{1 2 3} {4 5 6} {7 8 9}}
set keypad2 {{0 0 1 0 0} {0 2 3 4 0} {5 6 7 8 9} {0 A B C 0} {0 0 D 0 0}}

puts [get_code $instructions $keypad1]
puts [get_code $instructions $keypad2]

