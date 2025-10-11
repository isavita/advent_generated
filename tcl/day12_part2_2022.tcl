
set f [open input.txt r]
set y 0
array set grid {}
array set dist {}
set as {}
while {[gets $f line] >= 0} {
    for {set x 0} {$x < [string length $line]} {incr x} {
        set c [string index $line $x]
        set grid($x,$y) $c
        if {$c eq "S"} {
            set sx $x; set sy $y
            set grid($x,$y) a
        } elseif {$c eq "E"} {
            set ex $x; set ey $y
            set grid($x,$y) z
        } elseif {$c eq "a"} {
            lappend as $x,$y
        }
    }
    incr y
}
close $f

set rows $y
for {set i 0} {$i < $rows} {incr i} {
    for {set j 0} {$j < 1000} {incr j} {
        set dist($j,$i) -1
    }
}
set dist($ex,$ey) 0
set pq [list [list $ex,$ey 0]]

while {[llength $pq]} {
    set idx 0
    set min [lindex $pq 0 1]
    for {set i 1} {$i < [llength $pq]} {incr i} {
        if {[lindex $pq $i 1] < $min} {
            set min [lindex $pq $i 1]
            set idx $i
        }
    }
    set item [lindex $pq $idx]
    set pq [lreplace $pq $idx $idx]
    lassign [split [lindex $item 0] ,] cx cy
    if {$dist($cx,$cy) < [lindex $item 1]} continue
    foreach d {{-1 0} {1 0} {0 -1} {0 1}} {
        lassign $d dx dy
        set nx [expr {$cx + $dx}]
        set ny [expr {$cy + $dy}]
        if {$nx < 0 || $ny < 0 || $ny >= $rows || ![info exist grid($nx,$ny)]} continue
        if {[expr {[scan $grid($cx,$cy) %c] - [scan $grid($nx,$ny) %c]}] > 1} continue
        set nd [expr {$dist($cx,$cy) + 1}]
        if {$dist($nx,$ny) == -1 || $nd < $dist($nx,$ny)} {
            set dist($nx,$ny) $nd
            lappend pq [list "$nx,$ny" $nd]
        }
    }
}

set best $dist($sx,$sy)
foreach p $as {
    lassign [split $p ,] ax ay
    if {$dist($ax,$ay) != -1 && $dist($ax,$ay) < $best} {
        set best $dist($ax,$ay)
    }
}
puts $best
