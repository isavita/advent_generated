proc reverseString {s} {
    return [string reverse $s]
}

proc rotateGrid {grid} {
    set n [llength $grid]
    set out {}
    for {set c 0} {$c < $n} {incr c} {
        set row ""
        for {set r [expr {$n - 1}]} {$r >= 0} {incr r -1} {
            append row [string index [lindex $grid $r] $c]
        }
        lappend out $row
    }
    return $out
}

proc flipGrid {grid} {
    set out {}
    foreach row $grid {
        lappend out [reverseString $row]
    }
    return $out
}

proc gridOrientations {grid} {
    set out {}
    set cur $grid
    for {set i 0} {$i < 4} {incr i} {
        lappend out $cur
        lappend out [flipGrid $cur]
        set cur [rotateGrid $cur]
    }
    return $out
}

proc parseTiles {filename} {
    set f [open $filename r]
    set text [string trim [read $f]]
    close $f

    set tiles {}
    foreach block [split [string map {"\n\n" "\u0001"} $text] "\u0001"] {
        set lines [split $block "\n"]
        regexp {Tile (\d+):} [lindex $lines 0] -> id
        set grid [lrange $lines 1 end]
        lappend tiles [dict create id $id grid $grid]
    }
    return $tiles
}

proc topEdge {grid} { return [lindex $grid 0] }
proc bottomEdge {grid} { return [lindex $grid end] }
proc leftEdge {grid} {
    set s ""
    foreach row $grid { append s [string index $row 0] }
    return $s
}
proc rightEdge {grid} {
    set s ""
    foreach row $grid { append s [string index $row end] }
    return $s
}

proc solveAssemble {tiles} {
    set n [expr {int(sqrt([llength $tiles]))}]
    array set placed {}
    array set used {}

    proc bt {pos n tilesName placedName usedName} {
        upvar 1 $tilesName tiles
        upvar 1 $placedName placed
        upvar 1 $usedName used

        if {$pos == $n*$n} { return 1 }
        set r [expr {$pos / $n}]
        set c [expr {$pos % $n}]

        for {set i 0} {$i < [llength $tiles]} {incr i} {
            set tile [lindex $tiles $i]
            set id [dict get $tile id]
            if {[info exists used($id)]} { continue }

            foreach orient [gridOrientations [dict get $tile grid]] {
                if {$r > 0} {
                    set topNeighbor $placed([expr {$r-1}],$c)
                    if {[topEdge $orient] ne [bottomEdge [lindex $topNeighbor 1]]} { continue }
                }
                if {$c > 0} {
                    set leftNeighbor $placed($r,[expr {$c-1}])
                    if {[leftEdge $orient] ne [rightEdge [lindex $leftNeighbor 1]]} { continue }
                }

                set placed($r,$c) [list $id $orient]
                set used($id) 1
                if {[bt [expr {$pos+1}] $n tiles placed used]} { return 1 }
                unset used($id)
                unset placed($r,$c)
            }
        }
        return 0
    }

    if {![bt 0 $n tiles placed used]} {
        error "assembly failed"
    }

    set assembled {}
    for {set r 0} {$r < $n} {incr r} {
        set row {}
        for {set c 0} {$c < $n} {incr c} {
            lappend row $placed($r,$c)
        }
        lappend assembled $row
    }
    return $assembled
}

proc stripBorders {grid} {
    set out {}
    for {set r 1} {$r < [expr {[llength $grid]-1}]} {incr r} {
        set row [lindex $grid $r]
        lappend out [string range $row 1 end-1]
    }
    return $out
}

proc buildImage {assembled} {
    set n [llength $assembled]
    set image {}
    for {set tr 0} {$tr < $n} {incr tr} {
        set strippedRow {}
        foreach cell [lindex $assembled $tr] {
            lappend strippedRow [stripBorders [lindex $cell 1]]
        }

        set innerSize [llength [lindex $strippedRow 0]]
        for {set ir 0} {$ir < $innerSize} {incr ir} {
            set line ""
            for {set tc 0} {$tc < $n} {incr tc} {
                append line [lindex [lindex $strippedRow $tc] $ir]
            }
            lappend image $line
        }
    }
    return $image
}

proc countHashes {grid} {
    set c 0
    foreach row $grid {
        for {set i 0} {$i < [string length $row]} {incr i} {
            if {[string index $row $i] eq "#"} { incr c }
        }
    }
    return $c
}

proc roughness {image} {
    set monster {
        {18 0}
        {0 1} {5 1} {6 1} {11 1} {12 1} {17 1} {18 1} {19 1}
        {1 2} {4 2} {7 2} {10 2} {13 2} {16 2}
    }
    set monsterCount [llength $monster]

    foreach g [gridOrientations $image] {
        set h [llength $g]
        set w [string length [lindex $g 0]]
        set found 0

        for {set y 0} {$y <= $h-3} {incr y} {
            for {set x 0} {$x <= $w-20} {incr x} {
                set ok 1
                foreach p $monster {
                    lassign $p dx dy
                    if {[string index [lindex $g [expr {$y+$dy}]] [expr {$x+$dx}]] ne "#"} {
                        set ok 0
                        break
                    }
                }
                if {$ok} { incr found }
            }
        }

        if {$found > 0} {
            return [expr {[countHashes $g] - $found * $monsterCount}]
        }
    }

    return -1
}

set tiles [parseTiles "input.txt"]
set assembled [solveAssemble $tiles]
set image [buildImage $assembled]
puts [roughness $image]
