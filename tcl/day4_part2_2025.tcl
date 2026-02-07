#!/usr/bin/env tclsh
set f [open "input.txt" r]
set grid {}
while {[gets $f line] >= 0} {
    set line [string trimright $line "\r\n"]
    lappend grid [split $line ""]
}
close $f
set R [llength $grid]
if {$R == 0} {puts "Total rolls removed: 0"; exit}
set C 0
foreach row $grid {
    set len [llength $row]
    if {$len > $C} {set C $len}
}
set removed 0
set changed 1
set dr {-1 -1 -1 0 0 1 1 1}
set dc {-1 0 1 -1 1 -1 0 1}
while {$changed} {
    set changed 0
    for {set r 0} {$r < $R} {incr r} {
        set row [lindex $grid $r]
        for {set c 0} {$c < $C} {incr c} {
            if {$c >= [llength $row]} continue
            if {[lindex $row $c] ne "@"} continue
            set cnt 0
            for {set k 0} {$k < 8} {incr k} {
                set nr [expr {$r + [lindex $dr $k]}]
                set nc [expr {$c + [lindex $dc $k]}]
                if {$nr < 0 || $nr >= $R || $nc < 0} continue
                set nrow [lindex $grid $nr]
                if {$nc >= [llength $nrow]} continue
                if {[lindex $nrow $nc] eq "@"} {incr cnt}
            }
            if {$cnt < 4} {
                set row [lreplace $row $c $c *]
                set grid [lreplace $grid $r $r $row]
                incr changed
            }
        }
    }
    for {set r 0} {$r < $R} {incr r} {
        set row [lindex $grid $r]
        set newrow {}
        foreach ch $row {
            if {$ch eq "*"} {
                lappend newrow .
                incr removed
            } else {
                lappend newrow $ch
            }
        }
        set grid [lreplace $grid $r $r $newrow]
    }
}
puts "Total rolls removed: $removed"