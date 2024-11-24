
#!/usr/bin/env tclsh

proc readHeightmap {filename} {
    set heightmap {}
    set file [open $filename r]
    while {[gets $file line] != -1} {
        set row {}
        foreach char [split $line ""] {
            lappend row [scan $char %d]
        }
        lappend heightmap $row
    }
    close $file
    return $heightmap
}

proc isLowPoint {heightmap x y} {
    set height [lindex $heightmap $y $x]
    set rows [llength $heightmap]
    set cols [llength [lindex $heightmap 0]]

    if {$x > 0 && [lindex $heightmap $y [expr {$x-1}]] <= $height} {return 0}
    if {$x < $cols-1 && [lindex $heightmap $y [expr {$x+1}]] <= $height} {return 0}
    if {$y > 0 && [lindex $heightmap [expr {$y-1}] $x] <= $height} {return 0}
    if {$y < $rows-1 && [lindex $heightmap [expr {$y+1}] $x] <= $height} {return 0}
    return 1
}

proc exploreBasin {heightmap x y visited} {
    upvar $visited visitedMap
    set rows [llength $heightmap]
    set cols [llength [lindex $heightmap 0]]

    if {[info exists visitedMap($x,$y)] || [lindex $heightmap $y $x] == 9} {
        return 0
    }

    set visitedMap($x,$y) 1
    set size 1

    foreach {dx dy} {0 -1  -1 0  0 1  1 0} {
        set newX [expr {$x + $dx}]
        set newY [expr {$y + $dy}]
        
        if {$newX >= 0 && $newX < $cols && $newY >= 0 && $newY < $rows} {
            incr size [exploreBasin $heightmap $newX $newY visitedMap]
        }
    }
    return $size
}

proc main {} {
    set heightmap [readHeightmap "input.txt"]
    set basinSizes {}
    array set visited {}

    set rows [llength $heightmap]
    set cols [llength [lindex $heightmap 0]]

    for {set y 0} {$y < $rows} {incr y} {
        for {set x 0} {$x < $cols} {incr x} {
            if {[isLowPoint $heightmap $x $y]} {
                set size [exploreBasin $heightmap $x $y visited]
                lappend basinSizes $size
            }
        }
    }

    set sortedBasins [lsort -integer -decreasing $basinSizes]
    set result [expr {[lindex $sortedBasins 0] * [lindex $sortedBasins 1] * [lindex $sortedBasins 2]}]
    puts $result
}

main
