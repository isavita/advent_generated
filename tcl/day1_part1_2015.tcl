set file [open "input.txt" r]
set input [read $file]
close $file
set floor 0
foreach c [split $input ""] {
    if {$c == "("} {
        incr floor
    } elseif {$c == ")"} {
        incr floor -1
    }
}
puts $floor