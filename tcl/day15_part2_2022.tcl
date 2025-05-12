
proc distress {sensorList maxcoord} {
    for {set x 0} {$x <= $maxcoord} {incr x} {
        set y 0
        while {$y <= $maxcoord} {
            set detected 0
            set max_skip 0
            foreach sensor $sensorList {
                lassign $sensor sx sy dist
                set mdist [expr {abs($sx - $x) + abs($sy - $y)}]
                if {$mdist <= $dist} {
                    set detected 1
                    set rem_dist [expr {$dist - abs($sx - $x)}]
                    set skip [expr {$sy + $rem_dist - $y}]
                    set max_skip [expr {max($max_skip, $skip)}]
                }
            }

            if {!$detected} {
                return [expr {$x * 4000000 + $y}]
            } else {
                set y [expr {$y + $max_skip + 1}]
            }
        }
    }
    return -1
}

proc main {} {
    set maxcoord 4000000
    set sensors {}
    set fileId [open "input.txt" r]
    set content [read $fileId]
    close $fileId

    foreach line [split $content "\n"] {
        if {[regexp {Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)} $line -> sx sy bx by]} {
            set sx [expr {$sx}]
            set sy [expr {$sy}]
            set bx [expr {$bx}]
            set by [expr {$by}]
            set dist [expr {abs($sx - $bx) + abs($sy - $by)}]
            lappend sensors [list $sx $sy $dist]
        }
    }

    puts [distress $sensors $maxcoord]
}

main
