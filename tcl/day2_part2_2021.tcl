set file [open "input.txt" r]
set horizontalPosition 0
set depth 0
set aim 0

while {[gets $file line] >= 0} {
    set command [split $line]
    set direction [lindex $command 0]
    set units [expr {[lindex $command 1]}]

    switch -- $direction {
        "forward" {
            set horizontalPosition [expr {$horizontalPosition + $units}]
            set depth [expr {$depth + $aim * $units}]
        }
        "down" {
            set aim [expr {$aim + $units}]
        }
        "up" {
            set aim [expr {$aim - $units}]
        }
    }
}

close $file
set product [expr {$horizontalPosition * $depth}]
puts $product