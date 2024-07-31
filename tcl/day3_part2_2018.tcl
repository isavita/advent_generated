set claims {}

proc readClaims {filename} {
    set claims {}
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        regexp {#(\d+) @ (\d+),(\d+): (\d+)x(\d+)} $line -> id x y width height
        lappend claims [list $id $x $y $width $height]
    }
    close $file
    return $claims
}

array set fabric {}

set claims [readClaims "input.txt"]

foreach claim $claims {
    set id [lindex $claim 0]
    set x [lindex $claim 1]
    set y [lindex $claim 2]
    set width [lindex $claim 3]
    set height [lindex $claim 4]
    for {set i $y} {$i < $y + $height} {incr i} {
        for {set j $x} {$j < $x + $width} {incr j} {
            incr fabric($i,$j)
        }
    }
}

foreach claim $claims {
    set id [lindex $claim 0]
    set x [lindex $claim 1]
    set y [lindex $claim 2]
    set width [lindex $claim 3]
    set height [lindex $claim 4]
    set overlap 0
    for {set i $y} {$i < $y + $height} {incr i} {
        for {set j $x} {$j < $x + $width} {incr j} {
            if {$fabric($i,$j) > 1} {
                set overlap 1
                break
            }
        }
        if {$overlap} {break}
    }
    if {!$overlap} {
        puts $id
        return
    }
}