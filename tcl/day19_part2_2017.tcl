
set f [open input.txt r]
if {$f == -1} {error "Could not open file"}
set grid {}
while {[gets $f line] >= 0} {
    lappend grid $line
}
close $f

set x 0
set y 0
for {set i 0} {$i < [string length [lindex $grid 0]]} {incr i} {
    if {[string index [lindex $grid 0] $i] == "|"} {
        set x $i
        break
    }
}

set dx 0
set dy 1
set steps 0

while {1} {
    if {$x < 0 || $x >= [string length [lindex $grid 0]] || $y < 0 || $y >= [llength $grid]} {
        break
    }

    set cell [string index [lindex $grid $y] $x]

    if {$cell == " "} {
        break
    }

    incr steps

    if {$cell == "+"} {
        if {$dx == 0} {
            if {$x > 0 && ([string index [lindex $grid $y] [expr {$x - 1}]] == "-" || [regexp {[A-Z]} [string index [lindex $grid $y] [expr {$x - 1}]]])} {
                set dx -1
                set dy 0
            } else {
                set dx 1
                set dy 0
            }
        } else {
            if {$y > 0 && ([string index [lindex $grid [expr {$y - 1}]] $x] == "|" || [regexp {[A-Z]} [string index [lindex $grid [expr {$y - 1}]] $x]])} {
                set dx 0
                set dy -1
            } else {
                set dx 0
                set dy 1
            }
        }
    }

    set x [expr {$x + $dx}]
    set y [expr {$y + $dy}]
}

puts $steps
