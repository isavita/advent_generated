
#!/usr/bin/tclsh

set OFFSETS {-1 0 0 -1 0 1 1 0}

proc getDists {sy sx} {
    global grid OFFSETS
    set dists [dict create "$sy,$sx" 0]
    set queue [list [list $sy $sx]]
    set qIdx 0
    while {$qIdx < [llength $queue]} {
        lassign [lindex $queue $qIdx] y x
        incr qIdx
        set d [dict get $dists "$y,$x"]
        foreach {dy dx} $OFFSETS {
            set ny [expr {$y + $dy}]
            set nx [expr {$x + $dx}]
            if {[info exists grid($ny,$nx)] && $grid($ny,$nx) eq "." && ![dict exists $dists "$ny,$nx"]} {
                dict set dists "$ny,$nx" [expr {$d + 1}]
                lappend queue [list $ny $nx]
            }
        }
    }
    return $dists
}

proc isEnemy {myId other} {
    global unitData
    if {![dict exists $unitData $other]} { return 0 }
    if {[dict get $unitData $other hp] <= 0} { return 0 }
    if {[dict get $unitData $other type] eq [dict get $unitData $myId type]} { return 0 }
    return 1
}

proc findEnemies {myId} {
    global unitData
    set enemies {}
    set myType [dict get $unitData $myId type]
    dict for {id data} $unitData {
        if {[dict get $data type] ne $myType && [dict get $data hp] > 0} {
            dict set enemies $id $data
        }
    }
    return $enemies
}

proc moveUnit {id} {
    global grid unitData OFFSETS
    set uy [dict get $unitData $id y]
    set ux [dict get $unitData $id x]
    
    foreach {dy dx} $OFFSETS {
        set ny [expr {$uy + $dy}]; set nx [expr {$ux + $dx}]
        if {[info exists grid($ny,$nx)] && [isEnemy $id $grid($ny,$nx)]} return
    }
    
    set distsFromU [getDists $uy $ux]
    set enemies [findEnemies $id]
    set reachableTargets {}
    dict for {eid edata} $enemies {
        set ey [dict get $edata y]; set ex [dict get $edata x]
        foreach {dy dx} $OFFSETS {
            set ny [expr {$ey + $dy}]; set nx [expr {$ex + $dx}]
            if {[dict exists $distsFromU "$ny,$nx"]} {
                lappend reachableTargets [list $ny $nx [dict get $distsFromU "$ny,$nx"]]
            }
        }
    }
    
    if {[llength $reachableTargets] == 0} return
    
    set minDist 1000000
    foreach rt $reachableTargets {
        if {[lindex $rt 2] < $minDist} { set minDist [lindex $rt 2] }
    }
    set candidates {}
    foreach rt $reachableTargets {
        if {[lindex $rt 2] == $minDist} { lappend candidates [list [lindex $rt 0] [lindex $rt 1]] }
    }
    set sortedTargets [lsort -integer -index 0 [lsort -integer -index 1 $candidates]]
    set T [lindex $sortedTargets 0]
    set distsFromT [getDists [lindex $T 0] [lindex $T 1]]
    
    set bestStep ""; set minDist 1000000
    foreach {dy dx} $OFFSETS {
        set ny [expr {$uy + $dy}]; set nx [expr {$ux + $dx}]
        if {[dict exists $distsFromT "$ny,$nx"]} {
            set d [dict get $distsFromT "$ny,$nx"]
            if {$d < $minDist} {
                set minDist $d
                set bestStep [list $ny $nx]
            }
        }
    }
    
    if {$bestStep ne ""} {
        lassign $bestStep ny nx
        set grid($uy,$ux) "."; set grid($ny,$nx) $id
        dict set unitData $id y $ny; dict set unitData $id x $nx
    }
}

proc attackEnemy {id} {
    global grid unitData OFFSETS
    set uy [dict get $unitData $id y]; set ux [dict get $unitData $id x]
    set bestEnemy {}
    foreach {dy dx} $OFFSETS {
        set ny [expr {$uy + $dy}]; set nx [expr {$ux + $dx}]
        if {![info exists grid($ny,$nx)]} continue
        set targetId $grid($ny,$nx)
        if {[isEnemy $id $targetId]} {
            if {$bestEnemy eq "" || [dict get $unitData $targetId hp] < [dict get $unitData $bestEnemy hp]} {
                set bestEnemy $targetId
            }
        }
    }
    if {$bestEnemy ne ""} {
        set newHP [expr {[dict get $unitData $bestEnemy hp] - [dict get $unitData $id power]}]
        dict set unitData $bestEnemy hp $newHP
        if {$newHP <= 0} { set grid([dict get $unitData $bestEnemy y],[dict get $unitData $bestEnemy x]) "." }
    }
}

proc compareUnits {id1 id2} {
    global unitData
    set u1 [dict get $unitData $id1]; set u2 [dict get $unitData $id2]
    if {[dict get $u1 y] != [dict get $u2 y]} { return [expr {[dict get $u1 y] < [dict get $u2 y] ? -1 : 1}] }
    return [expr {[dict get $u1 x] < [dict get $u2 x] ? -1 : 1}]
}

set f [open "input.txt" r]; set y 0; set uid 0
while {[gets $f line] >= 0} {
    for {set x 0} {$x < [string length $line]} {incr x} {
        set char [string index $line $x]
        if {$char eq "E" || $char eq "G"} {
            set id [incr uid]
            dict set unitData $id [dict create y $y x $x type $char hp 200 power 3]
            set grid($y,$x) $id
        } else { set grid($y,$x) $char }
    }
    incr y
}
close $f

set rounds 0
while {1} {
    set aliveIds {}
    dict for {id data} $unitData { if {[dict get $data hp] > 0} { lappend aliveIds $id } }
    set unitIds [lsort -command compareUnits $aliveIds]
    set fullRound 1
    foreach id $unitIds {
        if {[dict get $unitData $id hp] <= 0} continue
        if {[llength [findEnemies $id]] == 0} { set fullRound 0; break }
        moveUnit $id; attackEnemy $id
    }
    if {!$fullRound} break
    incr rounds
}

set hpSum 0
dict for {id data} $unitData { if {[dict get $data hp] > 0} { incr hpSum [dict get $data hp] } }
puts [expr {$rounds * $hpSum}]
