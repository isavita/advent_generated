set file [open "input.txt" r]
set horizontalPosition 0
set depth 0

while {[gets $file line] >= 0} {
    set command [split $line]
    set direction [lindex $command 0]
    set units [expr {[lindex $command 1]}]

    switch -- $direction {
        forward {set horizontalPosition [expr {$horizontalPosition + $units}]}
        down    {set depth [expr {$depth + $units}]}
        up      {set depth [expr {$depth - $units}]}
    }
}

close $file
puts [expr {$horizontalPosition * $depth}]