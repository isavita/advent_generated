
proc main {} {
    set bricks [readInput "input.txt"]
    set bricks [settleBricks $bricks]

    set supports [dict create]
    set supportedBy [dict create]
    buildSupportMaps bricks supports supportedBy

    set safeToDisintegrate 0
    for {set i 0} {$i < [llength $bricks]} {incr i} {
        if {[isSafeToDisintegrate $i $supports $supportedBy]} {
            incr safeToDisintegrate
        }
    }
    puts "Part 1: $safeToDisintegrate"

    set totalChainReaction 0
    for {set i 0} {$i < [llength $bricks]} {incr i} {
        incr totalChainReaction [calculateChainReaction $i $supports $supportedBy]
    }
    puts "Part 2: $totalChainReaction"
}

proc readInput {filename} {
    set bricks [list]
    set f [open $filename r]
    while {[gets $f line] >= 0} {
        lappend bricks [parseBrick $line]
    }
    close $f
    return $bricks
}

proc parseBrick {line} {
    set parts [split $line "~"]
    set startCoords [split [lindex $parts 0] ","]
    set endCoords [split [lindex $parts 1] ","]
    set x1 [lindex $startCoords 0]
    set y1 [lindex $startCoords 1]
    set z1 [lindex $startCoords 2]
    set x2 [lindex $endCoords 0]
    set y2 [lindex $endCoords 1]
    set z2 [lindex $endCoords 2]
    return [list [expr {min($x1, $x2)}] [expr {min($y1, $y2)}] [expr {min($z1, $z2)}] [expr {max($x1, $x2)}] [expr {max($y1, $y2)}] [expr {max($z1, $z2)}]]
}

proc bricksOverlap {b1 b2} {
    set b1minX [lindex $b1 0]
    set b1minY [lindex $b1 1]
    set b1maxX [lindex $b1 3]
    set b1maxY [lindex $b1 4]

    set b2minX [lindex $b2 0]
    set b2minY [lindex $b2 1]
    set b2maxX [lindex $b2 3]
    set b2maxY [lindex $b2 4]

    return [expr {max($b1minX, $b2minX) <= min($b1maxX, $b2maxX) && \
                 max($b1minY, $b2minY) <= min($b1maxY, $b2maxY)}]
}

proc settleBricks {bricks} {
    set bricks [lsort -integer -index 2 $bricks]

    for {set i 0} {$i < [llength $bricks]} {incr i} {
        set brick [lindex $bricks $i]
        set currentMinZ [lindex $brick 2]
        set currentMaxZ [lindex $brick 5]

        set maxZ 1
        for {set j 0} {$j < $i} {incr j} {
            set other [lindex $bricks $j]
            if {[bricksOverlap $brick $other]} {
                set otherMaxZ [lindex $other 5]
                set maxZ [expr {max($maxZ, $otherMaxZ + 1)}]
            }
        }

        set diff [expr {$currentMinZ - $maxZ}]
        lset bricks $i 2 [expr {$currentMinZ - $diff}]
        lset bricks $i 5 [expr {$currentMaxZ - $diff}]
    }

    set bricks [lsort -integer -index 2 $bricks]
    return $bricks
}

proc buildSupportMaps {bricksVar supportsVar supportedByVar} {
    upvar 1 $bricksVar bricks $supportsVar supports $supportedByVar supportedBy

    for {set i 0} {$i < [llength $bricks]} {incr i} {
        dict set supports $i [list]
        dict set supportedBy $i [list]
    }

    for {set j 0} {$j < [llength $bricks]} {incr j} {
        set upper [lindex $bricks $j]
        set upperMinZ [lindex $upper 2]
        for {set i 0} {$i < $j} {incr i} {
            set lower [lindex $bricks $i]
            set lowerMaxZ [lindex $lower 5]

            if {[bricksOverlap $lower $upper] && $upperMinZ == [expr {$lowerMaxZ + 1}]} {
                dict lappend supports $i $j
                dict lappend supportedBy $j $i
            }
        }
    }
}

proc isSafeToDisintegrate {brickIndex supports supportedBy} {
    if {![dict exists $supports $brickIndex]} {
        return 1
    }
    foreach supportedBrick [dict get $supports $brickIndex] {
        set supporters [dict get $supportedBy $supportedBrick]
        if {[llength $supporters] == 1 && [lindex $supporters 0] == $brickIndex} {
            return 0
        }
    }
    return 1
}

proc calculateChainReaction {brickIndex supports supportedBy} {
    set fallen [dict create $brickIndex 1]

    set q [list]
    if {[dict exists $supports $brickIndex]} {
        set q [dict get $supports $brickIndex]
    }

    while {[llength $q] > 0} {
        set nextQ [list]
        set currentQ $q
        set q [list]

        foreach b $currentQ {
            if {![dict exists $fallen $b]} {
                 set allSupportingFallen 1
                 foreach supporter [dict get $supportedBy $b] {
                     if {![dict exists $fallen $supporter]} {
                         set allSupportingFallen 0
                         break
                     }
                 }

                if {$allSupportingFallen} {
                    dict set fallen $b 1
                    if {[dict exists $supports $b]} {
                        lappend nextQ {*}[dict get $supports $b]
                    }
                }
            }
        }
        set q [lsort -unique -integer $nextQ]
    }

    return [expr {[dict size $fallen] - 1}]
}

main
