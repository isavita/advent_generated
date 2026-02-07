
set f [open input.txt]
set data [read $f]
close $f
set data [regsub -all {,} $data " "]
set sum 0

proc is_invalid {x} {
    set s $x
    set n [string length $s]
    if {$n <= 1} {return 0}
    for {set p 1} {$p <= $n/2} {incr p} {
        if {$n % $p} continue
        set ok 1
        for {set i $p} {$i < $n && $ok} {incr i} {
            if {[string index $s $i] ne [string index $s [expr {$i % $p}]]} {set ok 0}
        }
        if {$ok} {return 1}
    }
    return 0
}

foreach token [split $data] {
    if {[regexp {^([0-9]+)-([0-9]+)$} $token -> a b]} {
        set a [expr {$a}]
        set b [expr {$b}]
        if {$a > $b} {set t $a; set a $b; set b $t}
        for {set x $a} {$x <= $b} {incr x} {
            if {[is_invalid $x]} {set sum [expr {$sum + $x}]}
            if {$x == 0xFFFFFFFFFFFFFFFF} break
        }
    }
}
puts $sum
