set f [open "input.txt" r]
set max_y 0
array set grid {}
while {[gets $f line] >= 0} {
    set pts [split [string map {" -> " " "} $line] " "]
    set prev ""
    foreach p $pts {
        if {$p eq ""} continue
        lassign [split $p ","] x y
        if {$y > $max_y} {set max_y $y}
        if {$prev ne ""} {
            lassign $prev px py
            set x1 [expr {$px < $x ? $px : $x}]
            set x2 [expr {$px > $x ? $px : $x}]
            set y1 [expr {$py < $y ? $py : $y}]
            set y2 [expr {$py > $y ? $py : $y}]
            for {set i $x1} {$i <= $x2} {incr i} {
                for {set j $y1} {$j <= $y2} {incr j} {
                    set grid($i,$j) 1
                }
            }
        }
        set prev [list $x $y]
    }
}
close $f
set sands 0
set first 0
while {1} {
    set sx 500
    set sy 0
    while {1} {
        set ny [expr {$sy + 1}]
        if {$sy == $max_y} {
            if {$first == 0} {set first $sands}
            set grid($sx,$sy) 1
            break
        }
        if {![info exists grid($sx,$ny)]} {
            set sy $ny
        } elseif {![info exists grid([expr {$sx-1}],$ny)]} {
            set sx [expr {$sx-1}]
            set sy $ny
        } elseif {![info exists grid([expr {$sx+1}],$ny)]} {
            set sx [expr {$sx+1}]
            set sy $ny
        } else {
            set grid($sx,$sy) 1
            break
        }
    }
    incr sands
    if {$sy == 0} break
}
puts $first