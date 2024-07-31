proc main {} {
    array set grid {}

    set file [open "input.txt" r]
    while {[gets $file line] >= 0} {
        regexp {(\d+),(\d+) -> (\d+),(\d+)} $line -> x1 y1 x2 y2
        if {$x1 == $x2} {
            set start [min $y1 $y2]
            set end [max $y1 $y2]
            for {set y $start} {$y <= $end} {incr y} {
                incr grid($x1,$y)
            }
        } elseif {$y1 == $y2} {
            set start [min $x1 $x2]
            set end [max $x1 $x2]
            for {set x $start} {$x <= $end} {incr x} {
                incr grid($x,$y1)
            }
        }
    }
    close $file

    set overlap 0
    foreach {key value} [array get grid] {
        if {$value >= 2} {
            incr overlap
        }
    }
    puts $overlap
}

proc min {a b} {return [expr {$a < $b ? $a : $b}]}
proc max {a b} {return [expr {$a > $b ? $a : $b}]}

main