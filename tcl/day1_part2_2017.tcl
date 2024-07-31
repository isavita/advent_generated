proc readInput {filename} {
    set file [open $filename r]
    set data [read $file]
    close $file
    return [split $data {}]
}

proc part1 {digits} {
    set sum 0
    set n [llength $digits]
    for {set i 0} {$i < $n} {incr i} {
        if {[lindex $digits $i] eq [lindex $digits [expr {($i + 1) % $n}]]} {
            set sum [expr {$sum + [lindex $digits $i]}]
        }
    }
    return $sum
}

proc part2 {digits} {
    set sum 0
    set n [llength $digits]
    set step [expr {$n / 2}]
    for {set i 0} {$i < $n} {incr i} {
        if {[lindex $digits $i] eq [lindex $digits [expr {($i + $step) % $n}]]} {
            set sum [expr {$sum + [lindex $digits $i]}]
        }
    }
    return $sum
}

set digits [readInput "input.txt"]
set result1 [part1 $digits]
set result2 [part2 $digits]

puts "Part 1: $result1"
puts "Part 2: $result2"