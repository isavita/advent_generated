set total 0
set file [open "input.txt" r]

while {[gets $file line] >= 0} {
    set dimensions [split $line "x"]
    if {[llength $dimensions] != 3} {
        error "Invalid input format"
    }
    set l [lindex $dimensions 0]
    set w [lindex $dimensions 1]
    set h [lindex $dimensions 2]

    set side1 [expr {$l * $w}]
    set side2 [expr {$w * $h}]
    set side3 [expr {$h * $l}]
    
    set smallest [expr {min($side1, min($side2, $side3))}]
    set total [expr {$total + 2 * $side1 + 2 * $side2 + 2 * $side3 + $smallest}]
}

close $file
puts $total

proc min {a b} {
    return [expr {$a < $b ? $a : $b}]
}