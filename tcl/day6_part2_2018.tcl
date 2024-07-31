proc readCoordinates {filename} {
    set coords {}
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        lappend coords [split $line ,]
    }
    close $file
    return $coords
}

proc manhattan {p1 p2} {
    return [expr {abs([lindex $p1 0] - [lindex $p2 0]) + abs([lindex $p1 1] - [lindex $p2 1])}]
}

proc min {a b} {
    return [expr {$a < $b ? $a : $b}]
}

proc max {a b} {
    return [expr {$a > $b ? $a : $b}]
}

proc largestFiniteArea {coords} {
    set minX [expr {inf}]
    set minY [expr {inf}]
    set maxX [expr {-inf}]
    set maxY [expr {-inf}]
    
    foreach coord $coords {
        set x [lindex $coord 0]
        set y [lindex $coord 1]
        set minX [min $minX $x]
        set minY [min $minY $y]
        set maxX [max $maxX $x]
        set maxY [max $maxY $y]
    }

    set areaCount [array set areas {}]
    set infiniteCoords {}
    
    for {set x $minX} {$x <= $maxX} {incr x} {
        for {set y $minY} {$y <= $maxY} {incr y} {
            set closest ""
            set minDist inf
            set ties 0
            
            foreach coord $coords {
                set dist [manhattan $coord [list $x $y]]
                if {$dist < $minDist} {
                    set closest $coord
                    set minDist $dist
                    set ties 0
                } elseif {$dist == $minDist} {
                    incr ties
                }
            }
            
            if {$ties == 1} {
                set key [join $closest ,]
                if {[info exists areas($key)]} {
                    incr areas($key)
                } else {
                    set areas($key) 1
                }
                if {[lindex $closest 0] == $minX || [lindex $closest 1] == $minY || 
                    [lindex $closest 0] == $maxX || [lindex $closest 1] == $maxY} {
                    lappend infiniteCoords $key
                }
            }
        }
    }
    
    set largest 0
    foreach {key value} [array get areas] {
        if {[lsearch -exact $infiniteCoords $key] == -1} {
            set largest [max $largest $value]
        }
    }
    return $largest
}

proc safeRegionSize {coords threshold} {
    set size 0
    set minX [expr {inf}]
    set minY [expr {inf}]
    set maxX [expr {-inf}]
    set maxY [expr {-inf}]
    
    foreach coord $coords {
        set x [lindex $coord 0]
        set y [lindex $coord 1]
        set minX [min $minX $x]
        set minY [min $minY $y]
        set maxX [max $maxX $x]
        set maxY [max $maxY $y]
    }

    for {set x $minX} {$x <= $maxX} {incr x} {
        for {set y $minY} {$y <= $maxY} {incr y} {
            set totalDist 0
            foreach coord $coords {
                set totalDist [expr {$totalDist + [manhattan $coord [list $x $y]]}]
            }
            if {$totalDist < $threshold} {
                incr size
            }
        }
    }
    return $size
}

set coords [readCoordinates "input.txt"]
puts "Largest finite area: [largestFiniteArea $coords]"
puts "Size of safe region: [safeRegionSize $coords 10000]"