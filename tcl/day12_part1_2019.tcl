proc abs {x} {
    if {$x < 0} {
        return [expr {-1 * $x}]
    }
    return $x
}

proc calculateEnergy {positions velocities} {
    set totalEnergy 0
    for {set i 0} {$i < [llength $positions]} {incr i} {
        set pos [lindex $positions $i]
        set vel [lindex $velocities $i]
        set potential [expr {[abs [lindex $pos 0]] + [abs [lindex $pos 1]] + [abs [lindex $pos 2]]}]
        set kinetic [expr {[abs [lindex $vel 0]] + [abs [lindex $vel 1]] + [abs [lindex $vel 2]]}]
        set totalEnergy [expr {$totalEnergy + $potential * $kinetic}]
    }
    return $totalEnergy
}

proc simulate {positions velocities steps} {
    for {set step 0} {$step < $steps} {incr step} {
        for {set i 0} {$i < [llength $positions]} {incr i} {
            for {set j [expr {$i + 1}]} {$j < [llength $positions]} {incr j} {
                set pos1 [lindex $positions $i]
                set pos2 [lindex $positions $j]
                set vel1 [lindex $velocities $i]
                set vel2 [lindex $velocities $j]

                for {set k 0} {$k < 3} {incr k} {
                    if {[lindex $pos1 $k] < [lindex $pos2 $k]} {
                        set vel1 [lreplace $vel1 $k $k [expr {[lindex $vel1 $k] + 1}]]
                        set vel2 [lreplace $vel2 $k $k [expr {[lindex $vel2 $k] - 1}]]
                    } elseif {[lindex $pos1 $k] > [lindex $pos2 $k]} {
                        set vel1 [lreplace $vel1 $k $k [expr {[lindex $vel1 $k] - 1}]]
                        set vel2 [lreplace $vel2 $k $k [expr {[lindex $vel2 $k] + 1}]]
                    }
                }
                set velocities [lreplace $velocities $i $i $vel1]
                set velocities [lreplace $velocities $j $j $vel2]
            }
        }

        for {set i 0} {$i < [llength $positions]} {incr i} {
            set pos [lindex $positions $i]
            set vel [lindex $velocities $i]
            for {set k 0} {$k < 3} {incr k} {
                set pos [lreplace $pos $k $k [expr {[lindex $pos $k] + [lindex $vel $k]}]]
            }
            set positions [lreplace $positions $i $i $pos]
        }
    }
    return [calculateEnergy $positions $velocities]
}

set file [open "input.txt" r]
set positions {}
set velocities {}

while {[gets $file line] >= 0} {
    if {[regexp {<x=(-?\d+), y=(-?\d+), z=(-?\d+)>} $line match x y z]} {
        lappend positions [list $x $y $z]
        lappend velocities [list 0 0 0]
    }
}
close $file

set totalEnergy [simulate $positions $velocities 1000]
puts $totalEnergy