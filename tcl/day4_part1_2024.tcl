
proc checkWord {grid word x y dx dy} {
    set len [string length $word]
    for {set i 0} {$i < $len} {incr i} {
        set newX [expr {$x + $dx * $i}]
        set newY [expr {$y + $dy * $i}]
        if {$newX < 0 || $newY < 0 || $newX >= [llength $grid] || $newY >= [string length [lindex $grid 0]]} {
            return 0
        }
        if {[string index [lindex $grid $newX] $newY] ne [string index $word $i]} {
            return 0
        }
    }
    return 1
}

proc countOccurrences {grid word} {
    set count 0
    set directions {0 1 1 0 1 1 -1 1 0 -1 -1 0 -1 -1 1 -1}
    set rows [llength $grid]
    set cols [string length [lindex $grid 0]]
    for {set i 0} {$i < $rows} {incr i} {
        for {set j 0} {$j < $cols} {incr j} {
            for {set k 0} {$k < 16} {incr k 2} {
                if {[checkWord $grid $word $i $j [lindex $directions $k] [lindex $directions [expr {$k+1}]]]} {
                    incr count
                }
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

set count [countOccurrences $grid "XMAS"]
puts "XMAS appears $count times in the word search"
