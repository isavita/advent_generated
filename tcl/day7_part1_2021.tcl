set file [open "input.txt" r]
set positions [split [read $file] ","] 
close $file

set positions [lmap pos $positions {expr {int($pos)}}]
set positions [lsort $positions]
set min_fuel [expr {2147483647}]

for {set i [lindex $positions 0]} {$i <= [lindex $positions end]} {incr i} {
    set fuel 0
    foreach pos $positions {
        set fuel [expr {$fuel + abs($pos - $i)}]
    }
    if {$fuel < $min_fuel} {
        set min_fuel $fuel
    }
}
puts $min_fuel

proc abs {n} {
    return [expr {$n < 0 ? -$n : $n}]
}