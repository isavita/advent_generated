set fp [open "input.txt" r]
set grid [split [string trimright [read $fp]] "\n"]
close $fp
set height [llength $grid]
set width [string length [lindex $grid 0]]
set nodes [list "1,0" "[expr {$width-2}],[expr {$height-1}]"]
for {set y 1} {$y < $height - 1} {incr y} {
    set row [lindex $grid $y]
    for {set x 1} {$x < $width - 1} {incr x} {
        if {[string index $row $x] ne "."} continue
        set nb 0
        foreach {dx dy} {0 1 0 -1 1 0 -1 0} {
            set nx [expr {$x+$dx}]; set ny [expr {$y+$dy}]
            if {$nx >= 0 && $nx < $width && $ny >= 0 && $ny < $height} {
                if {[string index [lindex $grid $ny] $nx] ne "#"} {incr nb}
            }
        }
        if {$nb > 2} {lappend nodes "$x,$y"}
    }
}
set id 0
foreach n $nodes {
    set n2i($n) $id
    set i2n($id) $n
    incr id
}
for {set i 0} {$i < $id} {incr i} {
    set ::adj($i) {}
    lassign [split $i2n($i) ,] sx sy
    set q [list [list $sx $sy 0]]
    set visited [dict create "$sx,$sy" 1]
    while {[llength $q] > 0} {
        set q [lassign $q curr]
        lassign $curr cx cy d
        if {$d > 0 && [info exists n2i($cx,$cy)]} {
            lappend ::adj($i) [list $n2i($cx,$cy) $d]
            continue
        }
        foreach {dx dy} {0 1 0 -1 1 0 -1 0} {
            set nx [expr {$cx+$dx}]; set ny [expr {$cy+$dy}]
            if {$nx < 0 || $nx >= $width || $ny < 0 || $ny >= $height} continue
            set char [string index [lindex $grid $ny] $nx]
            if {$char eq "#" || [dict exists $visited "$nx,$ny"]} continue
            if {($char eq "^" && $dy != -1) || ($char eq "v" && $dy != 1) || \
                ($char eq "<" && $dx != -1) || ($char eq ">" && $dx != 1)} continue
            dict set visited "$nx,$ny" 1
            lappend q [list $nx $ny [expr {$d+1}]]
        }
    }
}
set ::target 1
proc dfs {u seen} {
    if {$u == $::target} {return 0}
    set bit [expr {1 << $u}]
    set seen [expr {$seen | $bit}]
    set max -1000000000
    foreach edge $::adj($u) {
        lassign $edge v w
        if {($seen & (1 << $v)) == 0} {
            set res [dfs $v $seen]
            if {$res > -1000000000} {
                set d [expr {$res + $w}]
                if {$d > $max} {set max $d}
            }
        }
    }
    return $max
}
interp recursionlimit {} 5000
puts [dfs 0 0]