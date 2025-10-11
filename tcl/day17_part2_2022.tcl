
#!/usr/bin/env tclsh

set width 7
set profileDepth 30
set rockShapes {
    {{0 0} {1 0} {2 0} {3 0}}
    {{1 0} {0 1} {1 1} {2 1} {1 2}}
    {{0 0} {1 0} {2 0} {2 1} {2 2}}
    {{0 0} {0 1} {0 2} {0 3}}
    {{0 0} {1 0} {0 1} {1 1}}
}
set rockPointsCount {4 5 5 4 4}

array set chamber {}
array set seen {}

proc getChamber {y} {
    global chamber
    if {[info exists chamber($y)]} {return $chamber($y)}
    return 0
}
proc setChamber {y mask} {
    global chamber
    set chamber($y) $mask
}

proc getProfile {highestY} {
    global width profileDepth
    set profile {}
    for {set x 0} {$x < $width} {incr x} {
        set found $profileDepth
        for {set dy 0} {$dy < $profileDepth} {incr dy} {
            set y [expr {$highestY - $dy}]
            if {$y <= 0} break
            set row [getChamber $y]
            if {$row & (1 << $x)} {
                set found $dy
                break
            }
        }
        lappend profile $found
    }
    return $profile
}

proc tryMove {rock dx dy highestY} {
    global width
    set new {}
    foreach p $rock {
        lassign $p px py
        set nx [expr {$px + $dx}]
        set ny [expr {$py + $dy}]
        if {$nx < 0 || $nx >= $width || $ny <= 0} {return ""}
        if {[getChamber $ny] & (1 << $nx)} {return ""}
        lappend new [list $nx $ny]
    }
    return $new
}

proc simulate {jet totalRocks} {
    global rockShapes rockPointsCount width profileDepth seen
    set jetLen [string length $jet]
    set highestY 0
    set jetIdx 0
    set rockNum 0
    set extra 0
    set cycleFound 0

    while {$rockNum < $totalRocks} {
        set rIdx [expr {$rockNum % 5}]
        set shape [lindex $rockShapes $rIdx]
        set ptsCount [lindex $rockPointsCount $rIdx]
        set rock {}
        set startY [expr {$highestY + 4}]
        foreach p $shape {
            lassign $p px py
            lappend rock [list [expr {$px + 2}] [expr {$py + $startY}]]
        }

        while {1} {
            set dir [string index $jet [expr {$jetIdx % $jetLen}]]
            incr jetIdx
            set moved [tryMove $rock [expr {$dir eq "<" ? -1 : 1}] 0 highestY]
            if {$moved ne ""} {set rock $moved}
            set moved [tryMove $rock 0 -1 highestY]
            if {$moved eq ""} break
            set rock $moved
        }

        foreach p $rock {
            lassign $p px py
            set old [getChamber $py]
            setChamber $py [expr {$old | (1 << $px)}]
            if {$py > $highestY} {set highestY $py}
        }

        if {!$cycleFound && $rockNum > $jetLen} {
            set key [list $rIdx [expr {$jetIdx % $jetLen}] [getProfile $highestY]]
            if {[info exists seen($key)]} {
                lassign $seen($key) prevRock prevHeight
                set cycleLen [expr {$rockNum - $prevRock}]
                set cycleHeight [expr {$highestY - $prevHeight}]
                set remaining [expr {$totalRocks - $rockNum - 1}]
                set cycles [expr {$remaining / $cycleLen}]
                if {$cycles > 0} {
                    set extra [expr {$cycles * $cycleHeight}]
                    incr rockNum [expr {$cycles * $cycleLen}]
                    set cycleFound 1
                }
            } else {
                set seen($key) [list $rockNum $highestY]
            }
        }
        incr rockNum
    }
    return [expr {$highestY + $extra}]
}

set fp [open "input.txt" r]
set jet [read $fp]
close $fp
set jet [string trim $jet]

set total 1000000000000
set height [simulate $jet $total]
puts $height
