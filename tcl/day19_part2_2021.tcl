proc readInput {filename} {
    set scanners {}
    set scanner {}
    set f [open $filename r]
    while {[gets $f line] != -1} {
        set line [string trim $line]
        if {[string match "---*" $line]} {
            if {[llength $scanner] > 0} {
                lappend scanners $scanner
                set scanner {}
            }
        } elseif {$line ne ""} {
            lappend scanner [split $line ,]
        }
    }
    if {[llength $scanner] > 0} {
        lappend scanners $scanner
    }
    close $f
    return $scanners
}

proc getRotations {} {
    set rotations {}
    set perms {0 1 2}
    foreach perm [list {0 1 2} {0 2 1} {1 0 2} {1 2 0} {2 0 1} {2 1 0}] {
        foreach signs [list {1 1 1} {1 1 -1} {1 -1 1} {1 -1 -1} {-1 1 1} {-1 1 -1} {-1 -1 1} {-1 -1 -1}] {
            set rot {{0 0 0} {0 0 0} {0 0 0}}
            for {set i 0} {$i < 3} {incr i} {
                lset rot $i [lindex $perm $i] [lindex $signs $i]
            }
            set det [expr {
                [lindex $rot 0 0] * ([lindex $rot 1 1] * [lindex $rot 2 2] - [lindex $rot 1 2] * [lindex $rot 2 1]) -
                [lindex $rot 0 1] * ([lindex $rot 1 0] * [lindex $rot 2 2] - [lindex $rot 1 2] * [lindex $rot 2 0]) +
                [lindex $rot 0 2] * ([lindex $rot 1 0] * [lindex $rot 2 1] - [lindex $rot 1 1] * [lindex $rot 2 0])
            }]
            if {$det == 1} {
                lappend rotations $rot
            }
        }
    }
    return $rotations
}

proc rotate {point rotation} {
    lassign $point x y z
    lassign $rotation r0 r1 r2
    set nx [expr {$x * [lindex $r0 0] + $y * [lindex $r0 1] + $z * [lindex $r0 2]}]
    set ny [expr {$x * [lindex $r1 0] + $y * [lindex $r1 1] + $z * [lindex $r1 2]}]
    set nz [expr {$x * [lindex $r2 0] + $y * [lindex $r2 1] + $z * [lindex $r2 2]}]
    return [list $nx $ny $nz]
}

proc add {p1 p2} {
    lassign $p1 x1 y1 z1
    lassign $p2 x2 y2 z2
    return [list [expr {$x1 + $x2}] [expr {$y1 + $y2}] [expr {$z1 + $z2}]]
}

proc subtract {p1 p2} {
    lassign $p1 x1 y1 z1
    lassign $p2 x2 y2 z2
    return [list [expr {$x1 - $x2}] [expr {$y1 - $y2}] [expr {$z1 - $z2}]]
}

proc manhattan {p1 p2} {
    lassign $p1 x1 y1 z1
    lassign $p2 x2 y2 z2
    return [expr {abs($x1 - $x2) + abs($y1 - $y2) + abs($z1 - $z2)}]
}

proc solve {scanners} {
    set rotations [getRotations]
    set aligned [list 0]
    set scannerPositions [dict create 0 {0 0 0}]
    set beacons [list]
    foreach beacon [lindex $scanners 0] {
        lappend beacons $beacon
    }
    set pending [list]
    for {set i 1} {$i < [llength $scanners]} {incr i} {
        lappend pending $i
    }
    while {[llength $pending] > 0} {
        set found 0
        foreach scanner $pending {
            foreach rot $rotations {
                set rotated [list]
                foreach point [lindex $scanners $scanner] {
                    lappend rotated [rotate $point $rot]
                }
                set deltas [dict create]
                foreach beacon $rotated {
                    foreach alignedBeacon $beacons {
                        set delta [subtract $alignedBeacon $beacon]
                        dict incr deltas $delta
                    }
                }
                set maxCount 0
                set maxDelta {}
                dict for {delta count} $deltas {
                    if {$count > $maxCount} {
                        set maxCount $count
                        set maxDelta $delta
                    }
                }
                if {$maxCount >= 12} {
                    dict set scannerPositions $scanner $maxDelta
                    foreach beacon $rotated {
                        set newBeacon [add $beacon $maxDelta]
                        if {$newBeacon ni $beacons} {
                            lappend beacons $newBeacon
                        }
                    }
                    lappend aligned $scanner
                    set pending [lsearch -all -inline -not $pending $scanner]
                    set found 1
                    break
                }
            }
            if {$found} {
                break
            }
        }
        if {!$found} {
            puts "No alignment found for remaining scanners."
            break
        }
    }
    set maxDistance 0
    set positions [dict values $scannerPositions]
    for {set i 0} {$i < [llength $positions]} {incr i} {
        for {set j [expr {$i + 1}]} {$j < [llength $positions]} {incr j} {
            set dist [manhattan [lindex $positions $i] [lindex $positions $j]]
            if {$dist > $maxDistance} {
                set maxDistance $dist
            }
        }
    }
    return $maxDistance
}

proc main {} {
    set scanners [readInput "input.txt"]
    set result [solve $scanners]
    puts $result
}

main