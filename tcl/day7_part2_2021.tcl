set file [open "input.txt" r]
set positions [split [read $file] ","]
close $file

set positions [lmap num $positions {expr {$num + 0}}]
set positions [lsort $positions]

set min_fuel [expr {2147483647}]
for {set i [lindex $positions 0]} {$i <= [lindex $positions end]} {incr i} {
    set fuel 0
    foreach pos $positions {
        set diff [expr {abs($pos - $i)}]
        set fuel [expr {$fuel + ($diff * ($diff + 1)) / 2}]
    }
    set min_fuel [expr {min($min_fuel, $fuel)}]
}
puts $min_fuel

proc abs {n} {
    if {$n < 0} {return -$n}
    return $n
}

proc min {a b} {
    if {$a < $b} {return $a}
    return $b
}