
set input [read [open input.txt r]]

proc solve {input} {
    global roomCoordToWantChar coordsInFrontOfRooms
    array set roomCoordToWantChar {
        2,3 A 3,3 A
        2,5 B 3,5 B
        2,7 C 3,7 C
        2,9 D 3,9 D
    }
    array set coordsInFrontOfRooms {
        1,3 1 1,5 1 1,7 1 1,9 1
    }

    set start [parseInput $input]
    set heap [list $start]
    set seen [dict create]

    while {[llength $heap] > 0} {
        set heap [lsort -integer -index 1 $heap]
        set node [lindex $heap 0]
        set heap [lrange $heap 1 end]

        set key [lindex $node 0]
        if {[dict exists $seen $key]} continue
        dict set seen $key 1

        if {[allDone $node]} {
            return [lindex $node 1]
        }

        set grid [lindex $node 2]
        set unsettled [getUnsettledCoords $grid]
        foreach coord $unsettled {
            set nextMoves [getNextPossibleMoves $grid $coord]
            foreach nextCoord $nextMoves {
                set cp [copy $grid]
                set ur [lindex $coord 0]
                set uc [lindex $coord 1]
                set nr [lindex $nextCoord 0]
                set nc [lindex $nextCoord 1]
                set char [lindex $cp $ur $uc]
                set energyUsed [lindex $node 1]
                set energyUsed [expr {$energyUsed + [calcEnergy $char $coord $nextCoord]}]
                lset cp $nr $nc $char
                lset cp $ur $uc "."
                set newKey [join $cp ""]
                lappend heap [list $newKey $energyUsed $cp]
            }
        }
    }
    error "no solution"
}

proc parseInput {input} {
    set lines [split [string trim $input] \n]
    set grid [list]
    foreach line $lines {
        lappend grid [split $line ""]
    }
    return [list [join $grid ""] 0 $grid]
}

proc allDone {node} {
    global roomCoordToWantChar
    set grid [lindex $node 2]
    foreach {coord want} [array get roomCoordToWantChar] {
        set r [lindex [split $coord ,] 0]
        set c [lindex [split $coord ,] 1]
        if {[lindex $grid $r $c] ne $want} {
            return 0
        }
    }
    return 1
}

proc getUnsettledCoords {grid} {
    global roomCoordToWantChar
    set unsettled [list]
    for {set col 1} {$col < [llength [lindex $grid 0]]} {incr col} {
        set ch [lindex $grid 1 $col]
        if {$ch in {A B C D}} {
            lappend unsettled [list 1 $col]
        }
    }
    foreach col {3 5 7 9} {
        set roomFullFromBack 1
        for {set row [expr {[llength $grid] - 2}]} {$row >= 2} {incr row -1} {
            set coord "$row,$col"
            set wantChar $roomCoordToWantChar($coord)
            set gotChar [lindex $grid $row $col]
            if {$gotChar ne "."} {
                if {$gotChar ne $wantChar} {
                    set roomFullFromBack 0
                    lappend unsettled [list $row $col]
                } elseif {$gotChar eq $wantChar && !$roomFullFromBack} {
                    lappend unsettled [list $row $col]
                }
            }
        }
    }
    return $unsettled
}

proc getNextPossibleMoves {grid coord} {
    global roomCoordToWantChar coordsInFrontOfRooms
    set ur [lindex $coord 0]
    set uc [lindex $coord 1]
    set unsettledChar [lindex $grid $ur $uc]
    set startedInHallway [expr {$ur == 1}]
    set possible [list]
    set queue [list $coord]
    set seen [dict create]
    while {[llength $queue] > 0} {
        set rc [lindex $queue 0]
        set queue [lrange $queue 1 end]
        set r [lindex $rc 0]
        set c [lindex $rc 1]
        set key "$r,$c"
        if {[dict exists $seen $key]} continue
        dict set seen $key 1
        if {$rc ne $coord} {
            if {![info exists coordsInFrontOfRooms($key)]} {
                set wantChar ""
                if {[info exists roomCoordToWantChar($key)]} {
                    set wantChar $roomCoordToWantChar($key)
                }
                if {$wantChar eq ""} {
                    if {!$startedInHallway} {
                        lappend possible $rc
                    }
                } elseif {$wantChar eq $unsettledChar} {
                    set isStuckAmphipod 0
                    set roomHasDeeperOpenSpaces 0
                    for {set rr [expr {$r + 1}]} {$rr < [llength $grid] - 1} {incr rr} {
                        set ch [lindex $grid $rr $c]
                        if {$ch eq "."} {
                            set roomHasDeeperOpenSpaces 1
                        }
                        if {$ch ne "." && $ch ne $unsettledChar} {
                            set isStuckAmphipod 1
                            break
                        }
                    }
                    if {!$roomHasDeeperOpenSpaces && !$isStuckAmphipod} {
                        lappend possible $rc
                    }
                }
            }
        }
        foreach dc {{-1 0} {1 0} {0 -1} {0 1}} {
            set nr [expr {$r + [lindex $dc 0]}]
            set nc [expr {$c + [lindex $dc 1]}]
            if {[lindex $grid $nr $nc] eq "."} {
                lappend queue [list $nr $nc]
            }
        }
    }
    return $possible
}

proc copy {grid} {
    set cp [list]
    foreach row $grid {
        lappend cp [list {*}$row]
    }
    return $cp
}

proc calcEnergy {char start end} {
    set dist [expr {abs([lindex $end 1] - [lindex $start 1])}]
    set dist2 [expr {[lindex $start 0] - 1 + [lindex $end 0] - 1}]
    set dist3 [expr {$dist + $dist2}]
    set energyPerType [dict create A 1 B 10 C 100 D 1000]
    return [expr {[dict get $energyPerType $char] * $dist3}]
}

puts [solve $input]
