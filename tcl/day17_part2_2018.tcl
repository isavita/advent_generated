interp recursionlimit {} 10000

proc is_solid {x y} {
    global clay water
    if {[info exists clay($x,$y)]} {return 1}
    if {[info exists water($x,$y)] && $water($x,$y) eq "~"} {return 1}
    return 0
}

proc fill {x y} {
    global clay water max_y
    if {$y > $max_y} return
    if {[info exists clay($x,$y)]} return
    if {[info exists water($x,$y)] && $water($x,$y) eq "~"} return
    
    set already_flowing [expr {[info exists water($x,$y)] && $water($x,$y) eq "|"}]
    set water($x,$y) "|"
    
    if {!$already_flowing} {
        fill $x [expr {$y + 1}]
    }
    
    if {[is_solid $x [expr {$y + 1}]]} {
        set lx $x
        set l_wall 0
        while {1} {
            set nx [expr {$lx - 1}]
            if {[info exists clay($nx,$y)]} {set l_wall 1; break}
            set already_f [expr {[info exists water($nx,$y)] && $water($nx,$y) eq "|"}]
            set water($nx,$y) "|"
            if {!$already_f} { fill $nx [expr {$y + 1}] }
            if {![is_solid $nx [expr {$y + 1}]]} break
            set lx $nx
        }
        
        set rx $x
        set r_wall 0
        while {1} {
            set nx [expr {$rx + 1}]
            if {[info exists clay($nx,$y)]} {set r_wall 1; break}
            set already_f [expr {[info exists water($nx,$y)] && $water($nx,$y) eq "|"}]
            set water($nx,$y) "|"
            if {!$already_f} { fill $nx [expr {$y + 1}] }
            if {![is_solid $nx [expr {$y + 1}]]} break
            set rx $nx
        }
        
        if {$l_wall && $r_wall} {
            for {set i $lx} {$i <= $rx} {incr i} {
                set water($i,$y) "~"
            }
        }
    }
}

set fp [open "input.txt" r]
set max_y 0
while {[gets $fp line] >= 0} {
    if {[regexp {x=(\d+), y=(\d+)\.\.(\d+)} $line -> x y1 y2]} {
        for {set y $y1} {$y <= $y2} {incr y} { set clay($x,$y) 1 }
        if {$y2 > $max_y} {set max_y $y2}
    } elseif {[regexp {y=(\d+), x=(\d+)\.\.(\d+)} $line -> y x1 x2]} {
        for {set x $x1} {$x <= $x2} {incr x} { set clay($x,$y) 1 }
        if {$y > $max_y} {set max_y $y}
    }
}
close $fp

fill 500 0

set res 0
foreach {k v} [array get water] {
    if {$v eq "~"} {
        incr res
    }
}
puts $res