set file [open "input.txt" r]
set instructions [split [read $file] "\n"]
close $file

proc getBathroomCode {instructions} {
    set keypad {{1 2 3} {4 5 6} {7 8 9}}
    set x 1
    set y 1
    set code ""

    foreach instruction $instructions {
        foreach move [split $instruction ""] {
            switch -- $move {
                U {if {$x > 0} {set x [expr {$x - 1}]} }
                D {if {$x < 2} {set x [expr {$x + 1}]} }
                L {if {$y > 0} {set y [expr {$y - 1}]} }
                R {if {$y < 2} {set y [expr {$y + 1}]} }
            }
        }
        set code "${code}[lindex [lindex $keypad $x] $y]"
    }
    return $code
}

puts [getBathroomCode $instructions]