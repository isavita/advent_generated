set file [open "input.txt" r]
set input [read $file]
close $file
set floor 0
set position 0
set length [string length $input]

for {set i 0} {$i < $length} {incr i} {
    set c [string index $input $i]
    if {$c == "("} {
        incr floor
    } elseif {$c == ")"} {
        incr floor -1
    }
    if {$floor == -1} {
        set position [expr {$i + 1}]
        break
    }
}
puts $position