
set f [open input.txt r]
set pts {}
set xs {}
set ys {}
while {[gets $f line] >= 0} {
    if {[scan $line "%d , %d" x y] != 2} continue
    lappend pts [list $x $y]
    lappend xs $x
    lappend ys $y
}
close $f
if {[llength $pts] == 0} {
    puts "Largest valid area: 0"
    exit 0
}
set xs [lsort -unique -integer $xs]
set ys [lsort -unique -integer $ys]
set ux [list]
set uy [list]
foreach x $xs {dict set ux $x [dict size $ux]}
foreach y $ys {dict set uy $y [dict size $uy]}
set W [expr {2 * [dict size $ux] + 1}]
set H [expr {2 * [dict size $uy] + 1}]
set colW [lrepeat $W 1]
set rowH [lrepeat $H 1]
for {set i 0} {$i < [expr {[llength $xs] - 1}]} {incr i} {
    set gap [expr {max(0, [lindex $xs [expr {$i+1}]] - [lindex $xs $i] - 1)}]
    lset colW [expr {2*$i+2}] $gap
}
for {set i 0} {$i < [expr {[llength $ys] - 1}]} {incr i} {
    set gap [expr {max(0, [lindex $ys [expr {$i+1}]] - [lindex $ys $i] - 1)}]
    lset rowH [expr {2*$i+2}] $gap
}
set grid [lrepeat $H [lrepeat $W 0]]
proc TOGRID {px py} {
    global ux uy
    return [list [expr {2*[dict get $ux $px]+1}] [expr {2*[dict get $uy $py]+1}]]
}
for {set i 0} {$i < [llength $pts]} {incr i} {
    lassign [lindex $pts $i] ax ay
    lassign [lindex $pts [expr {($i+1)%[llength $pts]}]] bx by
    lassign [TOGRID $ax $ay] gx1 gy1
    lassign [TOGRID $bx $by] gx2 gy2
    if {$gx1 == $gx2} {
        set y0 [expr {min($gy1,$gy2)}]
        set y1 [expr {max($gy1,$gy2)}]
        for {set y $y0} {$y <= $y1} {incr y} {
            if {[lindex $rowH $y] > 0} {lset grid $y $gx1 1}
        }
    } else {
        set x0 [expr {min($gx1,$gx2)}]
        set x1 [expr {max($gx1,$gx2)}]
        for {set x $x0} {$x <= $x1} {incr x} {
            if {[lindex $colW $x] > 0} {lset grid $gy1 $x 1}
        }
    }
}
set queue [list [list 0 0]]
lset grid 0 0 2
set dirs {{0 1} {0 -1} {1 0} {-1 0}}
while {[llength $queue]} {
    set cur [lindex $queue 0]
    set queue [lrange $queue 1 end]
    lassign $cur cx cy
    foreach d $dirs {
        lassign $d dx dy
        set nx [expr {$cx+$dx}]
        set ny [expr {$cy+$dy}]
        if {$nx>=0 && $nx<$W && $ny>=0 && $ny<$H && [lindex $grid $ny $nx]==0} {
            lset grid $ny $nx 2
            lappend queue [list $nx $ny]
        }
    }
}
set P [lrepeat $H [lrepeat $W 0]]
for {set y 0} {$y < $H} {incr y} {
    for {set x 0} {$x < $W} {incr x} {
        set val [expr {[lindex $grid $y $x]!=2 ? [lindex $colW $x]*[lindex $rowH $y] : 0}]
        set left [expr {$x ? [lindex $P $y [expr {$x-1}]] : 0}]
        set up [expr {$y ? [lindex $P [expr {$y-1}] $x] : 0}]
        set diag [expr {$x&&$y ? [lindex $P [expr {$y-1}] [expr {$x-1}]] : 0}]
        lset P $y $x [expr {$val + $left + $up - $diag}]
    }
}
set maxArea 0
for {set i 0} {$i < [llength $pts]} {incr i} {
    for {set j $i} {$j < [llength $pts]} {incr j} {
        lassign [lindex $pts $i] ax ay
        lassign [lindex $pts $j] bx by
        set w [expr {abs($ax-$bx)+1}]
        set h [expr {abs($ay-$by)+1}]
        set area [expr {$w*$h}]
        if {$area <= $maxArea} continue
        lassign [TOGRID $ax $ay] gx1 gy1
        lassign [TOGRID $bx $by] gx2 gy2
        if {$gx1>$gx2} {set t $gx1; set gx1 $gx2; set gx2 $t}
        if {$gy1>$gy2} {set t $gy1; set gy1 $gy2; set gy2 $t}
        set total [lindex $P $gy2 $gx2]
        set left [expr {$gx1 ? [lindex $P $gy2 [expr {$gx1-1}]] : 0}]
        set up [expr {$gy1 ? [lindex $P [expr {$gy1-1}] $gx2] : 0}]
        set diag [expr {$gx1&&$gy1 ? [lindex $P [expr {$gy1-1}] [expr {$gx1-1}]] : 0}]
        set valid [expr {$total - $left - $up + $diag}]
        if {$valid == $area} {set maxArea $area}
    }
}
puts "Largest valid area: $maxArea"
