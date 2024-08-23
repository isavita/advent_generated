proc readInput {filename} {
    set file [open $filename r]
    set lines [split [read $file] "\n"]
    close $file
    return $lines
}

proc shuffleDeck {instructions deckSize} {
    set position 2019
    foreach line $instructions {
        if {[string match "deal into new stack" $line]} {
            set position [expr {$deckSize - 1 - $position}]
        } elseif {[string match "cut *" $line]} {
            set n [lindex [split $line] 1]
            set position [expr {($position - $n + $deckSize) % $deckSize}]
        } elseif {[string match "deal with increment *" $line]} {
            set increment [lindex [split $line] 3]
            set position [expr {($position * $increment) % $deckSize}]
        }
    }
    return $position
}

set instructions [readInput "input.txt"]
set deckSize 10007
set position [shuffleDeck $instructions $deckSize]
puts $position