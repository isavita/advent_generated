
#!/usr/bin/env tclsh

proc readHappinessValues {filename} {
    set happinessMap {}
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        set parts [split $line " "]
        if {[llength $parts] < 11} continue

        set from [lindex $parts 0]
        set to [string trimright [lindex $parts 10] "."]
        set change [lindex $parts 3]
        
        if {[lindex $parts 2] eq "lose"} {
            set change [expr {-$change}]
        }

        dict lappend happinessMap $from $to $change
    }
    close $file
    return $happinessMap
}

proc addYourself {happinessMapVar} {
    upvar $happinessMapVar happinessMap
    set guests [dict keys $happinessMap]
    
    foreach guest $guests {
        dict lappend happinessMap You $guest 0
        dict lappend happinessMap $guest You 0
    }
}

proc calculateHappiness {arrangement happinessMap} {
    set happiness 0
    set n [llength $arrangement]
    
    for {set i 0} {$i < $n} {incr i} {
        set left [expr {($i + $n - 1) % $n}]
        set right [expr {($i + 1) % $n}]
        
        set current [lindex $arrangement $i]
        set leftGuest [lindex $arrangement $left]
        set rightGuest [lindex $arrangement $right]
        
        set happiness [expr {$happiness + 
            [dict get $happinessMap $current $leftGuest] + 
            [dict get $happinessMap $current $rightGuest]
        }]
    }
    
    return $happiness
}

proc findOptimalArrangement {guests happinessMap} {
    set maxHappiness 0
    
    foreach perm [permutations $guests] {
        set happiness [calculateHappiness $perm $happinessMap]
        if {$happiness > $maxHappiness} {
            set maxHappiness $happiness
        }
    }
    
    return $maxHappiness
}

proc permutations {list} {
    if {[llength $list] <= 1} {
        return [list $list]
    }
    
    set result {}
    foreach item $list {
        set rest [lsearch -all -inline -not $list $item]
        foreach subperm [permutations $rest] {
            lappend result [concat $item $subperm]
        }
    }
    
    return $result
}

# Main program
set happinessMap [readHappinessValues "input.txt"]
addYourself happinessMap
set guests [dict keys $happinessMap]
set maxHappiness [findOptimalArrangement $guests $happinessMap]
puts $maxHappiness
