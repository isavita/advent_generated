
#!/usr/bin/env tclsh

proc parseInput {filename} {
    set deps {}
    set allSteps {}
    
    set file [open $filename r]
    while {[gets $file line] != -1} {
        if {[regexp {Step (\w) must be finished before step (\w) can begin.} $line _ a b]} {
            dict lappend deps $b $a
            dict set allSteps $a 1
            dict set allSteps $b 1
        }
    }
    close $file
    
    return [list $deps $allSteps]
}

proc topologicalSort {deps allSteps} {
    set order {}
    set available {}
    
    # Find initial available steps
    foreach step [dict keys $allSteps] {
        if {![dict exists $deps $step]} {
            lappend available $step
        }
    }
    set available [lsort $available]
    
    while {[llength $available] > 0} {
        # Pick the step that comes first alphabetically
        set next [lindex $available 0]
        set available [lreplace $available 0 0]
        lappend order $next
        
        # Update dependencies
        foreach step [dict keys $allSteps] {
            if {[dict exists $deps $step]} {
                set stepDeps [dict get $deps $step]
                if {[set idx [lsearch $stepDeps $next]] != -1} {
                    set stepDeps [lreplace $stepDeps $idx $idx]
                    if {[llength $stepDeps] == 0} {
                        dict unset deps $step
                        lappend available $step
                    } else {
                        dict set deps $step $stepDeps
                    }
                }
            }
        }
        
        set available [lsort $available]
    }
    
    return [join $order ""]
}

# Main execution
set input [parseInput "input.txt"]
set order [topologicalSort [lindex $input 0] [lindex $input 1]]
puts $order
