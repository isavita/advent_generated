set file [open "input.txt" r]
set cubes {}

while {[gets $file line] >= 0} {
    lappend cubes $line
}
close $file

set surfaceArea 0
set cubeSet [dict create]

foreach cube $cubes {
    dict set cubeSet $cube 1
}

foreach cube $cubes {
    set exposedSides 6
    set coords [split $cube ,]
    set x [lindex $coords 0]
    set y [lindex $coords 1]
    set z [lindex $coords 2]

    set neighbors [list \
        [format "%d,%d,%d" [expr {$x + 1}] $y $z] \
        [format "%d,%d,%d" [expr {$x - 1}] $y $z] \
        [format "%d,%d,%d" $x [expr {$y + 1}] $z] \
        [format "%d,%d,%d" $x [expr {$y - 1}] $z] \
        [format "%d,%d,%d" $x $y [expr {$z + 1}]] \
        [format "%d,%d,%d" $x $y [expr {$z - 1}]]]

    foreach neighbor $neighbors {
        if {[dict exists $cubeSet $neighbor]} {
            incr exposedSides -1
        }
    }
    incr surfaceArea $exposedSides
}

puts $surfaceArea