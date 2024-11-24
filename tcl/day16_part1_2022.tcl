
#!/usr/bin/env tclsh

# Read input file
proc readInput {filename} {
    set fp [open $filename r]
    set data [read $fp]
    close $fp
    return [string trim $data]
}

# Create valve data structure
proc parseValves {input} {
    array set valves {}
    foreach line [split $input "\n"] {
        regexp {Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)} $line _ id flow tunnels
        set valves($id,flow) $flow
        set valves($id,tunnels) [split $tunnels ", "]
    }
    return [array get valves]
}

# Floyd-Warshall algorithm for shortest paths
proc computeDistances {valvesArr} {
    upvar $valvesArr valves
    array set distances {}
    
    # Initialize distances
    foreach id [array names valves *,tunnels] {
        set src [lindex [split $id ","] 0]
        set distances($src,$src) 0
        foreach tunnel $valves($id) {
            set distances($src,$tunnel) 1
        }
    }
    
    # Compute shortest paths
    foreach k [array names valves *,tunnels] {
        set k [lindex [split $k ","] 0]
        foreach i [array names valves *,tunnels] {
            set i [lindex [split $i ","] 0]
            foreach j [array names valves *,tunnels] {
                set j [lindex [split $j ","] 0]
                if {[info exists distances($i,$k)] && [info exists distances($k,$j)]} {
                    set direct [expr {[info exists distances($i,$j)] ? $distances($i,$j) : 1000}]
                    set via [expr {$distances($i,$k) + $distances($k,$j)}]
                    if {$via < $direct} {
                        set distances($i,$j) $via
                    }
                }
            }
        }
    }
    return [array get distances]
}

# Recursive pressure maximization
proc maxPressure {valvesArr distances start time openValves} {
    upvar $valvesArr valves
    upvar $distances dist
    
    set maxPress 0
    foreach next $openValves {
        set travelTime [expr {$dist($start,$next) + 1}]
        set timeLeft [expr {$time - $travelTime}]
        
        if {$timeLeft > 0} {
            set pressure [expr {$timeLeft * $valves($next,flow)}]
            set remainingValves [lsearch -all -inline -not $openValves $next]
            
            set subPress [maxPressure valves dist $next $timeLeft $remainingValves]
            set totalPress [expr {$pressure + $subPress}]
            
            set maxPress [expr {max($maxPress, $totalPress)}]
        }
    }
    return $maxPress
}

# Main procedure
proc solve {} {
    # Read and parse input
    set input [readInput "input.txt"]
    array set valves [parseValves $input]
    array set distances [computeDistances valves]
    
    # Find valves with non-zero flow
    set openValves {}
    foreach id [array names valves *,flow] {
        set id [lindex [split $id ","] 0]
        if {$valves($id,flow) > 0} {
            lappend openValves $id
        }
    }
    
    # Solve
    set result [maxPressure valves distances "AA" 30 $openValves]
    puts $result
}

# Run solution
solve
