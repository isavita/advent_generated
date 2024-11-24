
#!/usr/bin/env tclsh

proc gcd {a b} {
    while {$b != 0} {
        set t $b
        set b [expr {$a % $b}]
        set a $t
    }
    return $a
}

proc lcm {a b} {
    return [expr {($a * $b) / [gcd $a $b]}]
}

proc applyGravity {moonsVar axis} {
    upvar $moonsVar moons
    set len [llength $moons]
    
    for {set i 0} {$i < $len} {incr i} {
        for {set j [expr {$i + 1}]} {$j < $len} {incr j} {
            set pos1 [lindex $moons $i 0]
            set pos2 [lindex $moons $j 0]
            set vel1 [lindex $moons $i 1]
            set vel2 [lindex $moons $j 1]
            
            switch $axis {
                x {
                    if {[lindex $pos1 0] > [lindex $pos2 0]} {
                        lset moons $i 1 0 [expr {[lindex $vel1 0] - 1}]
                        lset moons $j 1 0 [expr {[lindex $vel2 0] + 1}]
                    } elseif {[lindex $pos1 0] < [lindex $pos2 0]} {
                        lset moons $i 1 0 [expr {[lindex $vel1 0] + 1}]
                        lset moons $j 1 0 [expr {[lindex $vel2 0] - 1}]
                    }
                }
                y {
                    if {[lindex $pos1 1] > [lindex $pos2 1]} {
                        lset moons $i 1 1 [expr {[lindex $vel1 1] - 1}]
                        lset moons $j 1 1 [expr {[lindex $vel2 1] + 1}]
                    } elseif {[lindex $pos1 1] < [lindex $pos2 1]} {
                        lset moons $i 1 1 [expr {[lindex $vel1 1] + 1}]
                        lset moons $j 1 1 [expr {[lindex $vel2 1] - 1}]
                    }
                }
                z {
                    if {[lindex $pos1 2] > [lindex $pos2 2]} {
                        lset moons $i 1 2 [expr {[lindex $vel1 2] - 1}]
                        lset moons $j 1 2 [expr {[lindex $vel2 2] + 1}]
                    } elseif {[lindex $pos1 2] < [lindex $pos2 2]} {
                        lset moons $i 1 2 [expr {[lindex $vel1 2] + 1}]
                        lset moons $j 1 2 [expr {[lindex $vel2 2] - 1}]
                    }
                }
            }
        }
    }
}

proc applyVelocity {moonsVar axis} {
    upvar $moonsVar moons
    set len [llength $moons]
    
    for {set i 0} {$i < $len} {incr i} {
        set pos [lindex $moons $i 0]
        set vel [lindex $moons $i 1]
        
        switch $axis {
            x {lset moons $i 0 0 [expr {[lindex $pos 0] + [lindex $vel 0]}]}
            y {lset moons $i 0 1 [expr {[lindex $pos 1] + [lindex $vel 1]}]}
            z {lset moons $i 0 2 [expr {[lindex $pos 2] + [lindex $vel 2]}]}
        }
    }
}

proc findCycle {moonsVar initialMoonsVar axis} {
    upvar $moonsVar moons
    upvar $initialMoonsVar initialMoons
    
    for {set steps 1} {1} {incr steps} {
        applyGravity moons $axis
        applyVelocity moons $axis
        
        set match 1
        for {set i 0} {$i < [llength $moons]} {incr i} {
            set moon [lindex $moons $i]
            set initialMoon [lindex $initialMoons $i]
            
            switch $axis {
                x {
                    if {[lindex $moon 0 0] != [lindex $initialMoon 0 0] || 
                        [lindex $moon 1 0] != [lindex $initialMoon 1 0]} {
                        set match 0
                        break
                    }
                }
                y {
                    if {[lindex $moon 0 1] != [lindex $initialMoon 0 1] || 
                        [lindex $moon 1 1] != [lindex $initialMoon 1 1]} {
                        set match 0
                        break
                    }
                }
                z {
                    if {[lindex $moon 0 2] != [lindex $initialMoon 0 2] || 
                        [lindex $moon 1 2] != [lindex $initialMoon 1 2]} {
                        set match 0
                        break
                    }
                }
            }
        }
        
        if {$match} {
            return $steps
        }
    }
}

set file [open "input.txt" r]
set content [read $file]
close $file

set moons {}
set initialMoons {}

foreach line [split $content "\n"] {
    if {[regexp {<x=(-?\d+), y=(-?\d+), z=(-?\d+)>} $line -> x y z]} {
        lappend moons [list [list $x $y $z] [list 0 0 0]]
        lappend initialMoons [list [list $x $y $z] [list 0 0 0]]
    }
}

set cycleX [findCycle moons initialMoons x]
set cycleY [findCycle moons initialMoons y]
set cycleZ [findCycle moons initialMoons z]

set lcmXY [lcm $cycleX $cycleY]
set result [lcm $lcmXY $cycleZ]

puts $result
