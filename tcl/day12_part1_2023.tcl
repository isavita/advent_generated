
#!/usr/bin/env tclsh

# Read input file
proc readInput {filename} {
    set fp [open $filename r]
    set data [read $fp]
    close $fp
    return [split [string trim $data] "\n"]
}

# Parse input into rows
proc parseInput {input} {
    set rows {}
    foreach line $input {
        lassign [split $line " "] springs groupStr
        set group [split $groupStr ","]
        lappend rows [list $springs $group]
    }
    return $rows
}

# Memoized recursive arrangement counting
proc countArrangementsRecursive {row iSprings iGroup iContiguousDamaged cacheVar} {
    upvar $cacheVar cache

    # Base case: reached end of springs
    if {$iSprings == [string length [lindex $row 0]]} {
        if {$iGroup == [llength [lindex $row 1]] && $iContiguousDamaged == 0} {
            return 1
        } elseif {$iGroup == [expr {[llength [lindex $row 1]] - 1}] && 
                  $iContiguousDamaged == [lindex [lindex $row 1] $iGroup]} {
            return 1
        }
        return 0
    }

    # Check cache
    set cacheKey "$iSprings,$iGroup,$iContiguousDamaged"
    if {[info exists cache($cacheKey)]} {
        return $cache($cacheKey)
    }

    set res 0
    set springs [lindex $row 0]
    set groups [lindex $row 1]
    set char [string index $springs $iSprings]

    # Handle operational or unknown springs
    if {$char eq "." || $char eq "?"} {
        if {$iContiguousDamaged == 0} {
            incr res [countArrangementsRecursive $row [expr {$iSprings + 1}] $iGroup 0 cache]
        } elseif {$iContiguousDamaged == [lindex $groups $iGroup]} {
            incr res [countArrangementsRecursive $row [expr {$iSprings + 1}] [expr {$iGroup + 1}] 0 cache]
        }
    }

    # Handle damaged or unknown springs
    if {$char eq "#" || $char eq "?"} {
        if {$iGroup < [llength $groups] && $iContiguousDamaged < [lindex $groups $iGroup]} {
            incr res [countArrangementsRecursive $row [expr {$iSprings + 1}] $iGroup [expr {$iContiguousDamaged + 1}] cache]
        }
    }

    # Cache and return result
    set cache($cacheKey) $res
    return $res
}

# Count arrangements for a single row
proc countArrangements {row} {
    array unset cache
    array set cache {}
    return [countArrangementsRecursive $row 0 0 0 cache]
}

# Solve the entire problem
proc solve {input} {
    set total 0
    foreach row [parseInput $input] {
        incr total [countArrangements $row]
    }
    return $total
}

# Main execution
set input [readInput "input.txt"]
puts [solve $input]
