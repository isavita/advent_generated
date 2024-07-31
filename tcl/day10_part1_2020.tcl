set f [open "input.txt" r]
set adapters [split [read $f] "\n"]
close $f

set joltageDiffs(1) 0
set joltageDiffs(3) 1
set previousJoltage 0

set sortedAdapters [lsort -integer $adapters]

foreach adapter $sortedAdapters {
    set diff [expr {$adapter - $previousJoltage}]
    incr joltageDiffs($diff)
    set previousJoltage $adapter
}

set product [expr {$joltageDiffs(1) * $joltageDiffs(3)}]
puts $product