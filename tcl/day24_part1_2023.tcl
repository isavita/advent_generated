
proc parseInput {input} {
    set points {}
    foreach line $input {
        scan $line "%f, %f, %f @ %f, %f, %f" x1 y1 z1 vx1 vy1 vz1
        lappend points [list [list $x1 $y1 $z1] [list $vx1 $vy1 $vz1]]
    }
    return $points
}

proc isIntersecting2D {p1 p2} {
    lassign $p1 pos1 vel1
    lassign $p2 pos2 vel2
    lassign $pos1 x1 y1 z1
    lassign $vel1 vx1 vy1 vz1
    lassign $pos2 x2 y2 z2
    lassign $vel2 vx2 vy2 vz2

    set det [expr {$vx1*$vy2 - $vx2*$vy1}]
    if {$det == 0} {
        return [list 0 {} 0 0]
    }
    set t1 [expr {($vy2*($x2-$x1) - $vx2*($y2-$y1)) / $det}]
    set t2 [expr {($vy1*($x2-$x1) - $vx1*($y2-$y1)) / $det}]
    set x [expr {$x1 + $vx1*$t1}]
    set y [expr {$y1 + $vy1*$t1}]
    return [list 1 [list $x $y 0] $t1 $t2]
}

proc solve {input min max} {
    set points [parseInput $input]
    set cnt 0
    for {set i 0} {$i < [llength $points]} {incr i} {
        for {set j 0} {$j < $i} {incr j} {
            set isect [isIntersecting2D [lindex $points $i] [lindex $points $j]]
            lassign $isect isIntersect coord t1 t2
            if {$isIntersect} {
                lassign $coord x y z
                if {$x >= $min && $x <= $max && $y >= $min && $y <= $max && $t1 >= 0 && $t2 >= 0} {
                    incr cnt
                }
            }
        }
    }
    return $cnt
}

set file [open "input.txt" r]
set input [split [read $file] "\n"]
close $file
puts [solve $input 200000000000000 400000000000000]
