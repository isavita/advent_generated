
array set parent {}
set points {}

proc manhattan_distance {p1 p2} {
    set x1 [lindex $p1 0]
    set y1 [lindex $p1 1]
    set z1 [lindex $p1 2]
    set t1 [lindex $p1 3]
    set x2 [lindex $p2 0]
    set y2 [lindex $p2 1]
    set z2 [lindex $p2 2]
    set t2 [lindex $p2 3]
    return [expr {abs($x1 - $x2) + abs($y1 - $y2) + abs($z1 - $z2) + abs($t1 - $t2)}]
}

proc find {x} {
    global parent
    if {$parent($x) != $x} {
        set parent($x) [find $parent($x)]
    }
    return $parent($x)
}

proc union {x y} {
    global parent
    set rootX [find $x]
    set rootY [find $y]
    if {$rootX != $rootY} {
        set parent($rootX) $rootY
    }
}

proc main {} {
    global points parent

    set fileId [open "input.txt" r]
    while {[gets $fileId line] != -1} {
        set coords [split $line ","]
        lappend points $coords
    }
    close $fileId

    set num_points [llength $points]
    for {set i 0} {$i < $num_points} {incr i} {
        set parent($i) $i
    }

    for {set i 0} {$i < $num_points} {incr i} {
        for {set j $i} {$j < $num_points} {incr j} {
            if {$i == $j} continue
            set p1 [lindex $points $i]
            set p2 [lindex $points $j]
            if {[manhattan_distance $p1 $p2] <= 3} {
                union $i $j
            }
        }
    }

    set constellation_count 0
    for {set i 0} {$i < $num_points} {incr i} {
        if {$parent($i) == $i} {
            incr constellation_count
        }
    }
    puts $constellation_count
}

main
