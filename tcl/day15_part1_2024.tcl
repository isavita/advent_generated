
#!/usr/bin/tclsh

set fp [open "input.txt" r]
set input [read $fp]
close $fp

set lines [split [string trim $input] "\n"]

set grid {}
set moves ""
set readingMap 1

foreach line $lines {
    if {$readingMap} {
        if {[string first "#" $line] != -1} {
            lappend grid $line
        } else {
            set readingMap 0
            append moves $line
        }
    } else {
        append moves $line
    }
}

set robotR -1
set robotC -1
set gridH [llength $grid]
if {$gridH > 0} {
    set gridW [string length [lindex $grid 0]]
    for {set r 0} {$r < $gridH} {incr r} {
        set row [lindex $grid $r]
        set c [string first "@" $row]
        if {$c != -1} {
            set robotR $r
            set robotC $c
            lset grid $r [string replace $row $c $c "."]
            break
        }
    }
}

set dirs(^) {-1 0}
set dirs(v) {1 0}
set dirs(<) {0 -1}
set dirs(>) {0 1}

proc pushBoxes {gridVarName r c dr dc} {
    upvar 1 $gridVarName grid

    set gridH [llength $grid]
    if {$gridH == 0} {return 0}
    set gridW [string length [lindex $grid 0]]

    set next_r [expr {$r + $dr}]
    set next_c [expr {$c + $dc}]

    if {$next_r < 0 || $next_r >= $gridH || $next_c < 0 || $next_c >= $gridW} {
        return 0
    }
    set cell_after_box [string index [lindex $grid $next_r] $next_c]
    if {$cell_after_box eq "#"} {
        return 0
    }

    if {$cell_after_box eq "O"} {
        if {![pushBoxes grid $next_r $next_c $dr $dc]} {
            return 0
        }
    }

    set cell_after_box_updated [string index [lindex $grid $next_r] $next_c]

    if {$cell_after_box_updated eq "."} {
        set nextRow [lindex $grid $next_r]
        lset grid $next_r [string replace $nextRow $next_c $next_c "O"]

        set currentRow [lindex $grid $r]
        lset grid $r [string replace $currentRow $c $c "."]

        return 1
    }

    return 0
}

set gridH [llength $grid]
if {$gridH > 0} {
    set gridW [string length [lindex $grid 0]]
    foreach move [split $moves ""] {
        if {![info exists dirs($move)]} continue

        set d $dirs($move)
        set dr [lindex $d 0]
        set dc [lindex $d 1]

        set nr [expr {$robotR + $dr}]
        set nc [expr {$robotC + $dc}]

        if {$nr < 0 || $nr >= $gridH || $nc < 0 || $nc >= $gridW} {
            continue
        }

        set nextCell [string index [lindex $grid $nr] $nc]

        if {$nextCell eq "#"} {
            continue
        } elseif {$nextCell eq "O"} {
            if {![pushBoxes grid $nr $nc $dr $dc]} {
                continue
            }
        }

        set currentRow [lindex $grid $robotR]
        lset grid $robotR [string replace $currentRow $robotC $robotC "."]

        set nextRow [lindex $grid $nr]
        lset grid $nr [string replace $nextRow $nc $nc "@"]

        set robotR $nr
        set robotC $nc
    }
}

set sum 0
set gridH [llength $grid]
if {$gridH > 0} {
     set gridW [string length [lindex $grid 0]]
    for {set r 0} {$r < $gridH} {incr r} {
        set row [lindex $grid $r]
        for {set c 0} {$c < $gridW} {incr c} {
            if {[string index $row $c] eq "O"} {
                set sum [expr {$sum + $r * 100 + $c}]
            }
        }
    }
}

puts $sum
