
set f [open "input.txt" r]
set target [string trim [read $f]]
close $f
set grid(0,0) 1
set x 0
set y 0
set dx 0
set dy -1
while 1 {
    if {$x == $y || ($x < 0 && $x == -$y) || ($x > 0 && $x == 1-$y)} {
        set tmp $dx
        set dx [expr {-$dy}]
        set dy $tmp
    }
    incr x $dx
    incr y $dy
    set value 0
    for {set dxx -1} {$dxx <= 1} {incr dxx} {
        for {set dyy -1} {$dyy <= 1} {incr dyy} {
            if {[info exists grid([expr {$x+$dxx}],[expr {$y+$dyy}])]} {
                incr value $grid([expr {$x+$dxx}],[expr {$y+$dyy}])
            }
        }
    }
    set grid($x,$y) $value
    if {$value > $target} {
        puts $value
        break
    }
}
