
set f [open input.txt r]
set nodes [dict create]
while {[gets $f line] != -1} {
    if {![regexp {/dev/grid/node-x(\d+)-y(\d+)\s+\d+T\s+(\d+)T\s+(\d+)T} $line -> x y u a]} continue
    dict set nodes $x,$y [list $u $a]
}
close $f

set w 0; set h 0
dict for {k _} $nodes {
    lassign [split $k ,] x y
    if {$x>$w} {set w $x}
    if {$y>$h} {set h $y}
}

proc findHole {} {
    global nodes
    dict for {k v} $nodes { if {[lindex $v 0]==0} {return $k} }
}

proc dim {} {
    global nodes w h
    return [list $w $h]
}

proc moves {goal from to} {
    global nodes
    lassign [dim] w h
    set depth($from) 0
    set q [list [list 0 $from]]
    while {[llength $q]} {
        set q [lsort -integer -index 0 $q]
        set item [lindex $q 0]
        set q [lrange $q 1 end]
        lassign $item d p
        if {$p eq $to} {return $d}
        set d [expr {$d+1}]
        lassign [split $p ,] x y
        foreach {dx dy} {0 1 0 -1 1 0 -1 0} {
            set nx [expr {$x+$dx}]; set ny [expr {$y+$dy}]
            set np "$nx,$ny"
            if {$nx<0||$ny<0||$nx>$w||$ny>$h} continue
            if {$np eq $goal} continue
            if {[lindex [dict get $nodes $np] 0]>400} continue
            if {![info exists depth($np)]||$d<$depth($np)} {
                set depth($np) $d
                lappend q [list $d $np]
            }
        }
    }
    error "no path"
}

set goal "$w,0"
set hole [findHole]
set sum 0
while {$goal ne "0,0"} {
    set next "[expr {[lindex [split $goal ,] 0]-1}],0"
    set m [moves $goal $hole $next]
    incr sum $m
    set hole $next
    set m [moves $goal $goal $hole]
    incr sum $m
    set tmp $goal
    set goal $hole
    set hole $tmp
}
puts $sum
