
proc nextSecret s {
    set x [expr {$s * 64}]
    set s [expr {$s ^ $x}]
    set s [expr {$s & 16777215}]
    set x [expr {$s / 32}]
    set s [expr {$s ^ $x}]
    set s [expr {$s & 16777215}]
    set x [expr {$s * 2048}]
    set s [expr {$s ^ $x}]
    set s [expr {$s & 16777215}]
    return $s
}

proc encodeChange4 {c1 c2 c3 c4} {
    return [expr {($c1+9) + ($c2+9)*19 + ($c3+9)*19*19 + ($c4+9)*19*19*19}]
}

set f [open "input.txt" r]
set initials {}
while {[gets $f line] >= 0} {
    if {$line eq ""} continue
    lappend initials $line
}
close $f

set numSteps 2000
set buyers {}
foreach initVal $initials {
    set prices {}
    set s $initVal
    for {set j 0} {$j <= $numSteps} {incr j} {
        lappend prices [expr {$s % 10}]
        if {$j < $numSteps} {
            set s [nextSecret $s]
        }
    }
    set changes {}
    for {set j 0} {$j < $numSteps} {incr j} {
        lappend changes [expr {[lindex $prices [expr {$j+1}]] - [lindex $prices $j]}]
    }
    lappend buyers [list $prices $changes]
}

set patternCount [expr {19 * 19 * 19 * 19}]
set globalSum [lrepeat $patternCount 0]

foreach b $buyers {
    lassign $b prices changes
    set localPrice [lrepeat $patternCount -1]
    for {set i 0} {[expr {$i+3}] < $numSteps} {incr i} {
        set c1 [lindex $changes $i]
        set c2 [lindex $changes [expr {$i+1}]]
        set c3 [lindex $changes [expr {$i+2}]]
        set c4 [lindex $changes [expr {$i+3}]]
        if {$c1 < -9 || $c1 > 9 || $c2 < -9 || $c2 > 9 ||
            $c3 < -9 || $c3 > 9 || $c4 < -9 || $c4 > 9} {
            continue
        }
        set idx [encodeChange4 $c1 $c2 $c3 $c4]
        if {[lindex $localPrice $idx] < 0} {
            lset localPrice $idx [lindex $prices [expr {$i+4}]]
        }
    }
    for {set idx 0} {$idx < $patternCount} {incr idx} {
        set p [lindex $localPrice $idx]
        if {$p >= 0} {
            lset globalSum $idx [expr {[lindex $globalSum $idx] + $p}]
        }
    }
}

set ans 0
foreach s $globalSum {
    if {$s > $ans} {
        set ans $s
    }
}

puts $ans
