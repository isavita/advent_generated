
#!/usr/bin/env tclsh

proc readInput {filename} {
    set distances {}
    set file [open $filename r]
    while {[gets $file line] != -1} {
        set parts [split $line " "]
        if {[llength $parts] != 5} continue
        
        set from [lindex $parts 0]
        set to [lindex $parts 2]
        set dist [lindex $parts 4]
        
        dict lappend distances $from $to $dist
        dict lappend distances $to $from $dist
    }
    close $file
    return $distances
}

proc getUniqueLocations {distances} {
    set locations {}
    dict for {from dests} $distances {
        lappend locations $from
        dict for {to _} $dests {
            if {[lsearch $locations $to] == -1} {
                lappend locations $to
            }
        }
    }
    return $locations
}

proc calculateRouteDistance {route distances} {
    set sum 0
    for {set i 0} {$i < [llength $route] - 1} {incr i} {
        set from [lindex $route $i]
        set to [lindex $route $i+1]
        set sum [expr {$sum + [dict get $distances $from $to]}]
    }
    return $sum
}

proc findShortestRoute {locations distances} {
    set minDistance -1
    
    # Generate all permutations
    foreach perm [permutations $locations] {
        set dist [calculateRouteDistance $perm $distances]
        if {$minDistance == -1 || $dist < $minDistance} {
            set minDistance $dist
        }
    }
    
    return $minDistance
}

proc permutations {list} {
    if {[llength $list] <= 1} {
        return [list $list]
    }
    
    set result {}
    foreach elem $list {
        set rest [lsearch -all -inline -not $list $elem]
        foreach subperm [permutations $rest] {
            lappend result [concat $elem $subperm]
        }
    }
    
    return $result
}

# Main execution
set distances [readInput "input.txt"]
set locations [getUniqueLocations $distances]
set minDistance [findShortestRoute $locations $distances]
puts $minDistance
