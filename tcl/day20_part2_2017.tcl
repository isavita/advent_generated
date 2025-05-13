
#!/usr/bin/env tclsh

proc parseInput {filePath} {
    set particles [list]
    set f [open $filePath r]
    while {[gets $f line] != -1} {
        if {[string trim $line] eq ""} continue
        if {[regexp {p=<([^>]+)>, v=<([^>]+)>, a=<([^>]+)>} $line _ p_str v_str a_str]} {
            set p_coords [split $p_str ,]
            set v_coords [split $v_str ,]
            set a_coords [split $a_str ,]
            set p_nums [list]
            foreach c $p_coords { lappend p_nums [expr {int($c)}] }
            set v_nums [list]
            foreach c $v_coords { lappend v_nums [expr {int($c)}] }
            set a_nums [list]
            foreach c $a_coords { lappend a_nums [expr {int($c)}] }
            lappend particles [dict create p $p_nums v $v_nums a $a_nums]
        }
    }
    close $f
    return $particles
}

proc updateParticle {particle} {
    set v [dict get $particle v]
    set a [dict get $particle a]
    set p [dict get $particle p]
    lassign $v vx vy vz
    lassign $a ax ay az
    lassign $p px py pz
    set nvx [expr {$vx + $ax}]
    set nvy [expr {$vy + $ay}]
    set nvz [expr {$vz + $az}]
    set npx [expr {$px + $nvx}]
    set npy [expr {$py + $nvy}]
    set npz [expr {$pz + $nvz}]
    set particle [dict set particle v [list $nvx $nvy $nvz]]
    set particle [dict set particle p [list $npx $npy $npz]]
    return $particle
}

proc manhattanDistance {position} {
    set dist 0
    foreach coord $position {
        incr dist [expr {abs($coord)}]
    }
    return $dist
}

proc findClosestParticle {particles} {
    set min_dist -1
    set closest_index -1
    for {set i 0} {$i < [llength $particles]} {incr i} {
        set particle [lindex $particles $i]
        set position [dict get $particle p]
        set dist [manhattanDistance $position]
        if {$closest_index == -1 || $dist < $min_dist} {
            set min_dist $dist
            set closest_index $i
        }
    }
    return $closest_index
}

proc resolveCollisions {particles steps} {
    set particlesLeft [list]
    foreach p $particles { lappend particlesLeft $p }

    for {set tick 0} {$tick < $steps} {incr tick} {
        set positions [dict create]
        for {set i 0} {$i < [llength $particlesLeft]} {incr i} {
            set particle [lindex $particlesLeft $i]
            set updatedParticle [updateParticle $particle]
            lset particlesLeft $i $updatedParticle
            set pos [dict get $updatedParticle p]
            set posKey [join $pos ,]
            dict lappend positions $posKey $i
        }

        set collidingIndices [list]
        dict for {posKey indices} $positions {
            if {[llength $indices] > 1} {
                foreach idx $indices { lappend collidingIndices $idx }
            }
        }

        set nextParticlesLeft [list]
        set isColliding [dict create]
        foreach idx $collidingIndices { dict set isColliding $idx 1 }

        for {set i 0} {$i < [llength $particlesLeft]} {incr i} {
            if {![dict exists $isColliding $i]} {
                lappend nextParticlesLeft [lindex $particlesLeft $i]
            }
        }
        set particlesLeft $nextParticlesLeft
    }
    return [llength $particlesLeft]
}

proc main {} {
    set particles [parseInput "input.txt"]
    set closestParticleIndex [findClosestParticle $particles]
    puts "Closest particle index: $closestParticleIndex"
    set remainingParticles [resolveCollisions $particles 100]
    puts "Remaining particles after collisions: $remainingParticles"
}

main
