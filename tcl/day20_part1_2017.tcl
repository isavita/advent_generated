
proc manhattan_distance {pos} {
    set sum 0
    foreach coord $pos {
        set sum [expr {$sum + abs($coord)}]
    }
    return $sum
}

proc update_particle {particle} {
    lassign $particle p v a
    
    # Update velocity
    set new_v {}
    foreach vel_comp $v acc_comp $a {
        lappend new_v [expr {$vel_comp + $acc_comp}]
    }
    
    # Update position
    set new_p {}
    foreach pos_comp $p vel_comp $new_v {
        lappend new_p [expr {$pos_comp + $vel_comp}]
    }
    
    return [list $new_p $new_v $a]
}

proc solve {} {
    set particles {}
    set particle_id 0
    
    # Read input from file
    if {[catch {open "input.txt" r} file_id]} {
        puts "Error opening input.txt: $file_id"
        return
    }
    
    while {[gets $file_id line] != -1} {
        regexp {p=<([^>]+)>, v=<([^>]+)>, a=<([^>]+)>} $line _ p_str v_str a_str
        
        set p [lmap x [split $p_str ,] {string trim $x}]
        set v [lmap x [split $v_str ,] {string trim $x}]
        set a [lmap x [split $a_str ,] {string trim $x}]
        
        lappend particles [list $p $v $a]
        incr particle_id
    }
    close $file_id
    
    # Simulate for a large number of ticks
    set num_ticks 1000
    for {set i 0} {$i < $num_ticks} {incr i} {
        set updated_particles {}
        foreach particle $particles {
            lappend updated_particles [update_particle $particle]
        }
        set particles $updated_particles
    }
    
    # Find the particle closest to <0,0,0>
    set closest_particle_id -1
    set min_distance Inf
    
    for {set i 0} {$i < [llength $particles]} {incr i} {
        set particle [lindex $particles $i]
        lassign $particle pos _ _
        set distance [manhattan_distance $pos]
        if {$distance < $min_distance} {
            set min_distance $distance
            set closest_particle_id $i
        }
    }
    
    puts $closest_particle_id
}

solve
