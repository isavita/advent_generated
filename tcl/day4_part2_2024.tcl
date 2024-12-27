
proc checkMAS {grid x y dx dy} {
    set word "MAS"
    set forward 1
    set backward 1
    for {set i 0} {$i < [string length $word]} {incr i} {
        set newX [expr {$x + $dx * $i}]
        set newY [expr {$y + $dy * $i}]
        if {$newX < 0 || $newY < 0 || $newX >= [llength $grid] || $newY >= [string length [lindex $grid 0]]} {
            set forward 0
            break
        }
        if {[string index [lindex $grid $newX] $newY] ne [string index $word $i]} {
            set forward 0
        }
    }
    for {set i 0} {$i < [string length $word]} {incr i} {
        set newX [expr {$x + $dx * $i}]
        set newY [expr {$y + $dy * $i}]
        if {$newX < 0 || $newY < 0 || $newX >= [llength $grid] || $newY >= [string length [lindex $grid 0]]} {
            set backward 0
            break
        }
        if {[string index [lindex $grid $newX] $newY] ne [string index $word [expr {[string length $word] - 1 - $i}]]} {
            set backward 0
        }
    }
    return [expr {$forward || $backward}]
}

proc checkXMAS {grid x y} {
    if {[checkMAS $grid [expr {$x - 1}] [expr {$y - 1}] 1 1] && [checkMAS $grid [expr {$x - 1}] [expr {$y + 1}] 1 -1]} {
        return 1
    }
    if {[checkMAS $grid [expr {$x + 1}] [expr {$y - 1}] -1 1] && [checkMAS $grid [expr {$x + 1}] [expr {$y + 1}] -1 -1]} {
        return 1
    }
    return 0
}

proc countXMASPatterns {grid} {
    set count 0
    if {[llength $grid] < 3 || [string length [lindex $grid 0]] < 3} {
        return 0
    }
    for {set i 1} {$i < [expr {[llength $grid] - 1}]} {incr i} {
        for {set j 1} {$j < [expr {[string length [lindex $grid $i]] - 1}]} {incr j} {
            if {[string index [lindex $grid $i] $j] eq "A" && [checkXMAS $grid $i $j]} {
                incr count
            }
        }
    }
    return $count
}

set file [open "input.txt" r]
set grid {}
while {[gets $file line] >= 0} {
    set line [string trim $line]
    if {$line ne ""} {
        lappend grid $line
    }
}
close $file

set count [countXMASPatterns $grid]
puts "X-MAS patterns appear $count times in the word search"
