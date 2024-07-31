proc isPrime {n} {
    if {$n < 2} {return 0}
    for {set i 2} {$i*$i <= $n} {incr i} {
        if {$n % $i == 0} {return 0}
    }
    return 1
}

set b [expr {57 * 100 + 100000}]
set c [expr {$b + 17000}]
set h 0

for {set x $b} {$x <= $c} {incr x 17} {
    if {! [isPrime $x]} {
        incr h
    }
}

puts $h