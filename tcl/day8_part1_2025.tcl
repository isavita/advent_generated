#!/usr/bin/env tclsh
set f [open "input.txt"]
set pts {}
while {[gets $f line] >= 0} {
    if {[regexp {^\s*([+-]?\d+)\s*,\s*([+-]?\d+)\s*,\s*([+-]?\d+)\s*$} $line -> x y z]} {
        lappend pts [list $x $y $z]
    }
}
close $f
set n [llength $pts]
if {$n < 2} {
    puts "Not enough points to form circuits."
    exit
}
set edges {}
for {set i 0} {$i < $n} {incr i} {
    set pi [lindex $pts $i]
    for {set j [expr {$i+1}]} {$j < $n} {incr j} {
        set pj [lindex $pts $j]
        set dx [expr {[lindex $pi 0] - [lindex $pj 0]}]
        set dy [expr {[lindex $pi 1] - [lindex $pj 1]}]
        set dz [expr {[lindex $pi 2] - [lindex $pj 2]}]
        set d [expr {$dx*$dx + $dy*$dy + $dz*$dz}]
        lappend edges [list $i $j $d]
    }
}
set edges [lsort -integer -index 2 $edges]
array set parent {}
array set sz {}
for {set i 0} {$i < $n} {incr i} {set parent($i) $i; set sz($i) 1}
proc find {x} {
    upvar parent parent
    while {$parent($x) != $x} {
        set parent($x) $parent($parent($x))
        set x $parent($x)
    }
    return $x
}
proc unite {a b} {
    upvar parent parent sz sz
    set ra [find $a]
    set rb [find $b]
    if {$ra == $rb} {return}
    if {$sz($ra) < $sz($rb)} {
        set tmp $ra; set ra $rb; set rb $tmp
    }
    set parent($rb) $ra
    set sz($ra) [expr {$sz($ra) + $sz($rb)}]
}
set limit [expr {[llength $edges] < 1000 ? [llength $edges] : 1000}]
for {set i 0} {$i < $limit} {incr i} {
    set e [lindex $edges $i]
    unite [lindex $e 0] [lindex $e 1]
}
set top {0 0 0}
for {set i 0} {$i < $n} {incr i} {
    if {$parent($i) == $i} {
        set s $sz($i)
        if {$s > [lindex $top 0]} {
            set top [list $s [lindex $top 0] [lindex $top 1]]
        } elseif {$s > [lindex $top 1]} {
            set top [list [lindex $top 0] $s [lindex $top 1]]
        } elseif {$s > [lindex $top 2]} {
            set top [list [lindex $top 0] [lindex $top 1] $s]
        }
    }
}
set result 1
foreach v $top {
    if {$v > 0} {set result [expr {$result * $v}]}
}
puts "Product of three largest circuit sizes: $result"