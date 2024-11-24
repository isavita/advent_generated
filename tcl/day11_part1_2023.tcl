
#!/usr/bin/env tclsh

proc abs {x} {
    return [expr {abs($x)}]
}

proc buildGrid {input} {
    set grid [dict create width [string length [lindex $input 0]] height [llength $input]]
    
    for {set y 0} {$y < [dict get $grid height]} {incr y} {
        set line [lindex $input $y]
        for {set x 0} {$x < [string length $line]} {incr x} {
            set char [string index $line $x]
            if {$char ne "."} {
                dict set grid data [list $x $y] $char
            }
        }
    }
    
    return $grid
}

proc getEmptyRows {grid} {
    set emptyRows {}
    for {set y 0} {$y < [dict get $grid height]} {incr y} {
        set isEmpty 1
        for {set x 0} {$x < [dict get $grid width]} {incr x} {
            if {[dict exists $grid data [list $x $y]]} {
                set isEmpty 0
                break
            }
        }
        if {$isEmpty} {
            lappend emptyRows $y
        }
    }
    return $emptyRows
}

proc getEmptyCols {grid} {
    set emptyCols {}
    for {set x 0} {$x < [dict get $grid width]} {incr x} {
        set isEmpty 1
        for {set y 0} {$y < [dict get $grid height]} {incr y} {
            if {[dict exists $grid data [list $x $y]]} {
                set isEmpty 0
                break
            }
        }
        if {$isEmpty} {
            lappend emptyCols $x
        }
    }
    return $emptyCols
}

proc calculateOffsets {emptyIndexes bound} {
    set offsets [lrepeat $bound 0]
    foreach idx $emptyIndexes {
        for {set i [expr {$idx + 1}]} {$i < $bound} {incr i} {
            lset offsets $i [expr {[lindex $offsets $i] + 1}]
        }
    }
    return $offsets
}

proc expandGrid {grid expansionFactor} {
    set emptyCols [getEmptyCols $grid]
    set emptyRows [getEmptyRows $grid]
    set numLinesToAdd [expr {$expansionFactor - 1}]

    set newGrid [dict create \
        width [expr {[dict get $grid width] + [llength $emptyCols] * $numLinesToAdd}] \
        height [expr {[dict get $grid height] + [llength $emptyRows] * $numLinesToAdd}] \
        data [dict create]]

    set dXs [calculateOffsets $emptyCols [dict get $grid width]]
    set dYs [calculateOffsets $emptyRows [dict get $grid height]]

    dict for {coord char} [dict get $grid data] {
        lassign $coord x y
        set newX [expr {$x + [lindex $dXs $x] * $numLinesToAdd}]
        set newY [expr {$y + [lindex $dYs $y] * $numLinesToAdd}]
        dict set newGrid data [list $newX $newY] $char
    }

    return $newGrid
}

proc calculateLength {coord1 coord2} {
    lassign $coord1 x1 y1
    lassign $coord2 x2 y2
    return [expr {abs($x2 - $x1) + abs($y2 - $y1)}]
}

proc solve {input} {
    set grid [buildGrid $input]
    set expandedGrid [expandGrid $grid 2]
    
    set res 0
    set alreadySeen {}
    dict for {coord1 _} [dict get $expandedGrid data] {
        foreach coord2 $alreadySeen {
            set length [calculateLength $coord1 $coord2]
            incr res $length
        }
        lappend alreadySeen $coord1
    }
    
    return $res
}

set fp [open "input.txt" r]
set input [split [read -nonewline $fp] "\n"]
close $fp

puts [solve $input]
