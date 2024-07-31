set fd [open "input.txt" r]
set steps [read $fd]
close $fd
set steps [string trim $steps]
set currentPos 0
set valueAfterZero 0

for {set i 1} {$i <= 50000000} {incr i} {
    set currentPos [expr {($currentPos + $steps) % $i}]
    if {$currentPos == 0} {
        set valueAfterZero $i
    }
    set currentPos [expr {$currentPos + 1}]
}

puts $valueAfterZero