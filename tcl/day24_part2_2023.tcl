#!/usr/bin/env tclsh
set fname "input.txt"
set f [open $fname]
set hail {}
while {[gets $f line] >= 0} {
    if {[regexp {^\s*$} $line]} continue
    regexp {^\s*([0-9-]+),\s*([0-9-]+),\s*([0-9-]+)\s*@\s*([0-9-]+),\s*([0-9-]+),\s*([0-9-]+)} $line -> px py pz vx vy vz
    lappend hail [list $px $py $pz $vx $vy $vz]
}
close $f
set n [llength $hail]

# Part 1
set min 2e14
set max 4e14
set cnt 0
for {set i 0} {$i < $n} {incr i} {
    set h1 [lindex $hail $i]
    foreach {p1x p1y p1z v1x v1y v1z} $h1 break
    for {set j [expr {$i+1}]} {$j < $n} {incr j} {
        set h2 [lindex $hail $j]
        foreach {p2x p2y p2z v2x v2y v2z} $h2 break
        set det [expr {$v1x*$v2y - $v1y*$v2x}]
        if {$det == 0} continue
        set t1 [expr {((($p2x-$p1x)*$v2y) - (($p2y-$p1y)*$v2x)) / double($det)}]
        set t2 [expr {((($p2x-$p1x)*$v1y) - (($p2y-$p1y)*$v1x)) / double($det)}]
        if {$t1 <= 0 || $t2 <= 0} continue
        set ix [expr {$p1x + $v1x*$t1}]
        set iy [expr {$p1y + $v1y*$t1}]
        if {$ix >= $min && $ix <= $max && $iy >= $min && $iy <= $max} {
            incr cnt
        }
    }
}
puts "Part 1: $cnt"

# Part 2
if {$n < 3} {
    puts "Part 2: Not enough data points."
    exit
}
set h0 [lindex $hail 0]
set h1 [lindex $hail 1]
set h2 [lindex $hail 2]
foreach {p0x p0y p0z v0x v0y v0z} $h0 break
foreach {p1x p1y p1z v1x v1y v1z} $h1 break
foreach {p2x p2y p2z v2x v2y v2z} $h2 break

proc matZero {} { return {0 0 0 0 0 0 0} }

set A [list]
for {set i 0} {$i < 6} {incr i} { lappend A [matZero] }

set dvx1 [expr {$v0x-$v1x}]
set dvy1 [expr {$v0y-$v1y}]
set dvz1 [expr {$v0z-$v1z}]
set dpx1 [expr {$p0x-$p1x}]
set dpy1 [expr {$p0y-$p1y}]
set dpz1 [expr {$p0z-$p1z}]
set dvx2 [expr {$v0x-$v2x}]
set dvy2 [expr {$v0y-$v2y}]
set dvz2 [expr {$v0z-$v2z}]
set dpx2 [expr {$p0x-$p2x}]
set dpy2 [expr {$p0y-$p2y}]
set dpz2 [expr {$p0z-$p2z}]

proc setA {A i j val} {
    lset A $i $j $val
    return $A
}
set A [setA $A 0 1 $dvz1]
set A [setA $A 0 2 [expr {-$dvy1}]]
set A [setA $A 0 4 $dpz1]
set A [setA $A 0 5 [expr {-$dpy1}]]

set A [setA $A 1 0 [expr {-$dvz1}]]
set A [setA $A 1 2 $dvx1]
set A [setA $A 1 3 [expr {-$dpz1}]]
set A [setA $A 1 5 $dpx1]

set A [setA $A 2 0 $dvy1]
set A [setA $A 2 1 [expr {-$dvx1}]]
set A [setA $A 2 3 $dpy1]
set A [setA $A 2 4 [expr {-$dpx1}]]

set A [setA $A 3 1 $dvz2]
set A [setA $A 3 2 [expr {-$dvy2}]]
set A [setA $A 3 4 $dpz2]
set A [setA $A 3 5 [expr {-$dpy2}]]

set A [setA $A 4 0 [expr {-$dvz2}]]
set A [setA $A 4 2 $dvx2]
set A [setA $A 4 3 [expr {-$dpz2}]]
set A [setA $A 4 5 $dpx2]

set A [setA $A 5 0 $dvy2]
set A [setA $A 5 1 [expr {-$dvx2}]]
set A [setA $A 5 3 $dpy2]
set A [setA $A 5 4 [expr {-$dpx2}]]

set A [lset A 0 6 [expr {($p0y*$v0z - $p0z*$v0y) - ($p1y*$v1z - $p1z*$v1y)}]]
set A [lset A 1 6 [expr {($p0z*$v0x - $p0x*$v0z) - ($p1z*$v1x - $p1x*$v1z)}]]
set A [lset A 2 6 [expr {($p0x*$v0y - $p0y*$v0x) - ($p1x*$v1y - $p1y*$v1x)}]]
set A [lset A 3 6 [expr {($p0y*$v0z - $p0z*$v0y) - ($p2y*$v2z - $p2z*$v2y)}]]
set A [lset A 4 6 [expr {($p0z*$v0x - $p0x*$v0z) - ($p2z*$v2x - $p2x*$v2z)}]]
set A [lset A 5 6 [expr {($p0x*$v0y - $p0y*$v0x) - ($p2x*$v2y - $p2y*$v2x)}]]

proc gauss {A} {
    for {set i 0} {$i < 6} {incr i} {
        set maxrow $i
        set maxval [expr {abs([lindex [lindex $A $i] $i])}]
        for {set k [expr {$i+1}]} {$k < 6} {incr k} {
            set val [expr {abs([lindex [lindex $A $k] $i])}]
            if {$val > $maxval} {
                set maxval $val
                set maxrow $k
            }
        }
        if {$maxrow != $i} {
            set tmp [lindex $A $i]
            set A [lreplace $A $i $i [lindex $A $maxrow]]
            set A [lreplace $A $maxrow $maxrow $tmp]
        }
        for {set k [expr {$i+1}]} {$k < 6} {incr k} {
            set factor [expr {[lindex [lindex $A $k] $i] / double([lindex [lindex $A $i] $i])}]
            for {set j $i} {$j <= 6} {incr j} {
                set newval [expr {[lindex [lindex $A $k] $j] - $factor * [lindex [lindex $A $i] $j]}]
                set row [lreplace [lindex $A $k] $j $j $newval]
                set A [lreplace $A $k $k $row]
            }
        }
    }
    for {set i 5} {$i >= 0} {incr i -1} {
        for {set j [expr {$i+1}]} {$j < 6} {incr j} {
            set rhs [expr {[lindex [lindex $A $i] 6] - [lindex [lindex $A $i] $j] * [lindex [lindex $A $j] 6]}]
            set row [lreplace [lindex $A $i] 6 6 $rhs]
            set A [lreplace $A $i $i $row]
        }
        set val [expr {[lindex [lindex $A $i] 6] / double([lindex [lindex $A $i] $i])}]
        set row [lreplace [lindex $A $i] 6 6 $val]
        set A [lreplace $A $i $i $row]
    }
    return $A
}
set A [gauss $A]
set prx [expr {round([lindex [lindex $A 0] 6])}]
set pry [expr {round([lindex [lindex $A 1] 6])}]
set prz [expr {round([lindex [lindex $A 2] 6])}]
puts "Part 2: [expr {$prx+$pry+$prz}]"