
#!/usr/bin/env tclsh

# Parse input from file
proc readInput {filename} {
    set fp [open $filename r]
    set data [read $fp]
    close $fp
    return [split [string trim $data] "\n"]
}

# Parse a line into springs and groups
proc parseLine {line} {
    lassign [split $line " "] springs groupStr
    set group [split $groupStr ","]
    return [list $springs $group]
}

# Memoized recursive arrangement counting
proc countArrangements {springs groups} {
    global memo
    
    # Create memoization key
    set key "[join $springs ""]:[join $groups ","]"
    
    # Check memoized result
    if {[info exists memo($key)]} {
        return $memo($key)
    }
    
    # Base cases
    if {[llength $groups] == 0} {
        return [expr {[string first "#" $springs] == -1 ? 1 : 0}]
    }
    
    if {[string length $springs] == 0} {
        return [expr {[llength $groups] == 0 ? 1 : 0}]
    }
    
    set result 0
    set firstChar [string index $springs 0]
    
    # Handle operational spring
    if {$firstChar in ". ?"} {
        incr result [countArrangements [string range $springs 1 end] $groups]
    }
    
    # Handle damaged spring
    if {$firstChar in "# ?"} {
        set firstGroup [lindex $groups 0]
        
        # Check if we can place a group of damaged springs
        if {[string length $springs] >= $firstGroup} {
            set canPlace 1
            for {set i 0} {$i < $firstGroup} {incr i} {
                if {[string index $springs $i] == "."} {
                    set canPlace 0
                    break
                }
            }
            
            # Ensure next character (if exists) is not a damaged spring
            if {$canPlace} {
                if {[string length $springs] == $firstGroup} {
                    # Reached end of springs
                    incr result [countArrangements "" [lrange $groups 1 end]]
                } elseif {[string index $springs $firstGroup] != "#"} {
                    # Place group and continue
                    incr result [countArrangements [string range $springs [expr {$firstGroup + 1}] end] [lrange $groups 1 end]]
                }
            }
        }
    }
    
    # Memoize and return
    set memo($key) $result
    return $result
}

# Unfold row
proc unfoldRow {row} {
    lassign $row springs groups
    
    set unfoldedSprings [join [lrepeat 5 $springs] "?"]
    set unfoldedGroups [concat {*}[lrepeat 5 $groups]]
    
    return [list $unfoldedSprings $unfoldedGroups]
}

# Main solve function
proc solve {input} {
    global memo
    array unset memo
    
    set total 0
    foreach line $input {
        set row [parseLine $line]
        set unfoldedRow [unfoldRow $row]
        incr total [countArrangements {*}$unfoldedRow]
    }
    
    return $total
}

# Main execution
set input [readInput "input.txt"]
puts [solve $input]
