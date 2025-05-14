
proc main {} {
    set filename "input.txt"
    set fd [open $filename r]

    array set weights {}
    array set children {}
    array set allPrograms {}
    array set childPrograms {}

    while {[gets $fd line] != -1} {
        set parts [split $line " "]
        set name [lindex $parts 0]
        set weightStr [lindex $parts 1]
        set weight [string range $weightStr 1 [expr {[string length $weightStr] - 2}]]
        set weights($name) $weight
        set allPrograms($name) 1

        if {[llength $parts] > 3} {
            set childList [lrange $parts 3 end]
            set cleanedChildren {}
            foreach child $childList {
                set cleanedChild [string trimright $child ","]
                lappend cleanedChildren $cleanedChild
                set childPrograms($cleanedChild) 1
            }
            set children($name) $cleanedChildren
        }
    }
    close $fd

    set bottomProgram ""
    foreach program [array names allPrograms] {
        if {![info exists childPrograms($program)]} {
            set bottomProgram $program
            break
        }
    }
    puts "Part 1: Bottom program is $bottomProgram"

    findUnbalancedWeight $bottomProgram weights children
}

proc findUnbalancedWeight {program weightsArray childrenArray} {
    upvar 1 $weightsArray weights
    upvar 1 $childrenArray children

    if {![info exists children($program)]} {
        return $weights($program)
    }

    set childList $children($program)
    array set weightMap {}
    set totalChildrenWeight 0

    foreach child $childList {
        set childTotalWeight [findUnbalancedWeight $child weights children]
        if {[info exists weightMap($childTotalWeight)]} {
            lappend weightMap($childTotalWeight) $child
        } else {
            set weightMap($childTotalWeight) [list $child]
        }
        set totalChildrenWeight [expr {$totalChildrenWeight + $childTotalWeight}]
    }

    if {[array size weightMap] > 1} {
        set correctWeight 0
        set wrongWeight 0
        set wrongProgram ""

        foreach totalWeight [array names weightMap] {
            set childrenAtWeight $weightMap($totalWeight)
            if {[llength $childrenAtWeight] == 1} {
                set wrongWeight $totalWeight
                set wrongProgram [lindex $childrenAtWeight 0]
            } else {
                set correctWeight $totalWeight
            }
        }

        set diff [expr {$correctWeight - $wrongWeight}]
        puts "Part 2: Program $wrongProgram should have weight [expr {$weights($wrongProgram) + $diff}]"
    }

    return [expr {$weights($program) + $totalChildrenWeight}]
}

main
