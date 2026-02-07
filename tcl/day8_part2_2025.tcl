#!/usr/bin/env tclsh
set f [open "input.txt" r]
set pts {}
while {[gets $f line] >= 0} {
    set line [string trim $line]
    if {$line eq ""} continue
    if {[regexp {^\s*([-\d]+),([-\d]+),([-\d]+)\s*$} $line -> x y z]} {
        lappend pts [list $x $y $z]
    }
}
close $f
set n [llength $pts]
if {$n < 2} exit
set edges {}
for {set i 0} {$i < $n} {incr i} {
    set pi [lindex $pts $i]
    for {set j [expr {$i+1}]} {$j < $n} {incr j} {
        set pj [lindex $pts $j]
        foreach {x1 y1 z1} $pi {}
        foreach {x2 y2 z2} $pj {}
        set dx [expr {$x1 - $x2}]
        set dy [expr {$y1 - $y2}]
        set dz [expr {$z1 - $z2}]
        set d [expr {$dx*$dx + $dy*$dy + $dz*$dz}]
        lappend edges [list $d $i $j]
    }
}
set edges [lsort -integer -index 0 $edges]

array set parent {}
array set rank {}
for {set i 0} {$i < $n} {incr i} {
    set parent($i) $i
    set rank($i) 0
}
proc find {parentVar x} {
    upvar $parentVar parent
    while {$parent($x) != $x} {
        set parent($x) $parent($parent($x))
        set x $parent($x)
    }
    return $x
}
proc unite {parentVar rankVar a b} {
    upvar $parentVar parent
    upvar $rankVar rank
    set ra $parent($a)
    set rb $parent($b)
    if {$ra == $rb} {return}
    if {$rank($ra) < $rank($rb)} {
        set parent($ra) $rb
    } elseif {$rank($ra) > $rank($rb)} {
        set parent($rb) $ra
    } else {
        set parent($rb) $ra
        incr rank($ra)
    }
}
set comps $n
foreach edge $edges {
    foreach {d u v} $edge {}
    set ru [find parent $u]
    set rv [find parent $v]
    if {$ru != $rv} {
        unite parent rank $ru $rv
        if {[incr comps -1] == 1} {
            set p1 [lindex $pts $u]
            set p2 [lindex $pts $v]
            foreach {x1 y1 z1} $p1 {}
            foreach {x2 y2 z2} $p2 {}
            puts "Connected $x1,$y1,$z1 and $x2,$y2,$z2"
            puts "Product of X coordinates: [expr {$x1*$x2}]"
            break
        }
    }
}
exit