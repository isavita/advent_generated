
set f [open input.txt r]
set y 0
array set tracks {}
set carts {}
while {[gets $f line] >= 0} {
    for {set x 0} {$x < [string length $line]} {incr x} {
        set c [string index $line $x]
        if {$c in {^ v < >}} {
            lappend carts [list $x $y $c left]
            set tracks($x,$y) [expr {$c in {^ v} ? "|" : "-"}]
        } elseif {$c in {"|" "-" "+" "/" "\\"}} {
            set tracks($x,$y) $c
        }
    }
    incr y
}
close $f

proc turnLeft {dir} {
    switch $dir {^ {return <} v {return >} < {return v} > {return ^}}
}
proc turnRight {dir} {
    switch $dir {^ {return >} v {return <} < {return ^} > {return v}}
}
proc moveCart {cartIdx} {
    global carts tracks
    lassign [lindex $carts $cartIdx] x y dir next
    switch $dir {
        ^ {incr y -1}
        v {incr y}
        < {incr x -1}
        > {incr x}
    }
    set track $tracks($x,$y)
    switch $track {
        / {set dir [expr {$dir in {^ v} ? [turnRight $dir] : [turnLeft $dir]}]}
        \\ {set dir [expr {$dir in {^ v} ? [turnLeft $dir] : [turnRight $dir]}]}
        + {
            switch $next {
                left {set dir [turnLeft $dir]; set next straight}
                straight {set next right}
                right {set dir [turnRight $dir]; set next left}
            }
        }
    }
    lset carts $cartIdx [list $x $y $dir $next]
}

while {[llength $carts] > 1} {
    set carts [lsort -index 1 -integer [lsort -index 0 -integer $carts]]
    set remove {}
    for {set i 0} {$i < [llength $carts]} {incr i} {
        if {$i in $remove} continue
        moveCart $i
        lassign [lindex $carts $i] x y
        for {set j 0} {$j < [llength $carts]} {incr j} {
            if {$i == $j} continue
            lassign [lindex $carts $j] jx jy
            if {$jx == $x && $jy == $y} {
                lappend remove $i $j
                break
            }
        }
    }
    set new {}
    for {set i 0} {$i < [llength $carts]} {incr i} {
        if {$i ni $remove} {lappend new [lindex $carts $i]}
    }
    set carts $new
}
lassign [lindex $carts 0] x y
puts "$x,$y"
