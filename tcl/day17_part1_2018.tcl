set input [open "input.txt" r]
set data [read $input]
close $input
set lines [split [string trim $data] "\n"]

set ground [list [list "+"]]
set maxX 0
set minX 0
set maxY 0
set minY 20
set xOffset 500
set yOffset 0

foreach line $lines {
    set split [regexp -all -inline {[^=, .]+} $line]
    if {[lindex $split 0] == "x"} {
        set x [expr {[lindex $split 1] - $xOffset}]
        set y1 [expr {[lindex $split 3] - $yOffset}]
        set y2 [expr {[lindex $split 4] - $yOffset}]
        
        while {$x >= $maxX} {
            incr maxX
            for {set j 0} {$j < [llength $ground]} {incr j} {
                lset ground $j [concat [lindex $ground $j] "."]
            }
        }
        while {$x <= $minX} {
            incr minX -1
            for {set j 0} {$j < [llength $ground]} {incr j} {
                lset ground $j [concat "." [lindex $ground $j]]
            }
        }
        while {$y2 > $maxY} {
            incr maxY
            lappend ground [lrepeat [llength [lindex $ground 0]] "."]
        }
        if {$y1 < $minY} {
            set minY $y1
        }
        for {set i $y1} {$i <= $y2} {incr i} {
            lset ground $i [expr {$x - $minX}] "#"
        }
    } else {
        set y [expr {[lindex $split 1] - $yOffset}]
        set x1 [expr {[lindex $split 3] - $xOffset}]
        set x2 [expr {[lindex $split 4] - $xOffset}]
        
        while {$y > $maxY} {
            incr maxY
            lappend ground [lrepeat [llength [lindex $ground 0]] "."]
        }
        while {$x2 >= $maxX} {
            incr maxX
            for {set j 0} {$j < [llength $ground]} {incr j} {
                lset ground $j [concat [lindex $ground $j] "."]
            }
        }
        while {$x1 <= $minX} {
            incr minX -1
            for {set j 0} {$j < [llength $ground]} {incr j} {
                lset ground $j [concat "." [lindex $ground $j]]
            }
        }
        for {set i $x1} {$i <= $x2} {incr i} {
            lset ground $y [expr {$i - $minX}] "#"
        }
        if {$y < $minY} {
            set minY $y
        }
    }
}

set water_count 0
set flow_count 0
set round_limit 200000

while {[lindex $ground 1 [expr {-$minX}]] ne "|" && $water_count < $round_limit} {
    set can_move 1
    set x [expr {-$minX}]
    set y 1
    set try_left 0
    while {$can_move} {
        if {$y + 1 > $maxY || [lindex $ground [expr {$y + 1}] $x] eq "|"} {
            lset ground $y $x "|"
            set can_move 0
            if {$y >= $minY} {
                incr flow_count
            }
        } elseif {[lindex $ground [expr {$y + 1}] $x] eq "."} {
            incr y
            set try_left 0
        } elseif {[lindex $ground [expr {$y + 1}] $x] in {"#" "~"}} {
            if {($try_left == 1 && [lindex $ground $y [expr {$x - 1}]] eq "|") || \
                ($try_left == 2 && [lindex $ground $y [expr {$x + 1}]] eq "|") || \
                ([lindex $ground $y [expr {$x + 1}]] eq "|" && [lindex $ground $y [expr {$x - 1}]] ne ".") || \
                ([lindex $ground $y [expr {$x + 1}]] ne "." && [lindex $ground $y [expr {$x - 1}]] eq "|")} {
                lset ground $y $x "|"
                incr flow_count
                set can_move 0
                for {set i [expr {$x + 1}]} {$i < [llength [lindex $ground 0]]} {incr i} {
                    if {[lindex $ground $y $i] eq "~"} {
                        lset ground $y $i "|"
                        incr water_count -1
                        incr flow_count 1
                    } else {
                        break
                    }
                }
                for {set i [expr {$x - 1}]} {$i >= 0} {incr i -1} {
                    if {[lindex $ground $y $i] eq "~"} {
                        lset ground $y $i "|"
                        incr water_count -1
                        incr flow_count 1
                    } else {
                        break
                    }
                }
            } elseif {($try_left == 0 && [lindex $ground $y [expr {$x - 1}]] eq ".") || \
                     ($try_left == 1 && [lindex $ground $y [expr {$x - 1}]] eq ".")} {
                incr x -1
                set try_left 1
            } elseif {($try_left == 0 && [lindex $ground $y [expr {$x + 1}]] eq ".") || \
                     ($try_left == 2 && [lindex $ground $y [expr {$x + 1}]] eq ".")} {
                incr x
                set try_left 2
            } else {
                set can_move 0
                lset ground $y $x "~"
                incr water_count
            }
        }
    }
}

puts [expr {$flow_count + $water_count}]