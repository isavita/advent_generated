proc readInput {filename} {
    set file [open $filename r]
    set happiness {}
    while {[gets $file line] >= 0} {
        regexp {(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+).} $line match person gainOrLose units neighbor
        set units [expr {$gainOrLose eq "lose" ? -$units : $units}]
        dict set happiness $person $neighbor $units
    }
    close $file
    return $happiness
}

proc permute {list} {
    if {[llength $list] <= 1} {
        return [list $list]
    }
    set result {}
    foreach item $list {
        set sublist [lrange $list 0 [expr {[lsearch $list $item] - 1}]]
        set sublist [concat $sublist [lrange $list [expr {[lsearch $list $item] + 1}] end]]
        foreach perm [permute $sublist] {
            lappend result [linsert $perm 0 $item]
        }
    }
    return $result
}

proc calculateHappiness {arrangement happiness} {
    set total 0
    set n [llength $arrangement]
    for {set i 0} {$i < $n} {incr i} {
        set person [lindex $arrangement $i]
        set left [lindex $arrangement [expr {($i - 1 + $n) % $n}]]
        set right [lindex $arrangement [expr {($i + 1) % $n}]]
        set total [expr {$total + [dict get $happiness $person $left] + [dict get $happiness $person $right]}]
    }
    return $total
}

proc findOptimalHappiness {happiness} {
    set people [dict keys $happiness]
    set permutations [permute $people]
    set maxHappiness -inf
    foreach arrangement $permutations {
        set happinessValue [calculateHappiness $arrangement $happiness]
        if {$happinessValue > $maxHappiness} {
            set maxHappiness $happinessValue
        }
    }
    return $maxHappiness
}

set happiness [readInput "input.txt"]
puts [findOptimalHappiness $happiness]