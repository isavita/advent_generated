
package require Tcl 8.6

proc readInput {filename} {
    set f [open $filename r]
    set content [read $f]
    close $f
    set lines [split [string trim $content] \n]
    set orderingRules {}
    set updates {}
    set isUpdateSection false

    foreach line $lines {
        set trimmedLine [string trim $line]
        if {$trimmedLine eq ""} {
            set isUpdateSection true
            continue
        }

        if {!$isUpdateSection} {
            set parts [split $trimmedLine |]
            if {[llength $parts] == 2} {
                set x [string trim [lindex $parts 0]]
                set y [string trim [lindex $parts 1]]
                if {[string is integer -strict $x] && [string is integer -strict $y]} {
                    lappend orderingRules [list $x $y]
                }
            }
        } else {
            set nums [split $trimmedLine ,]
            set currentUpdate {}
            foreach numStr $nums {
                set trimmedNumStr [string trim $numStr]
                if {[string is integer -strict $trimmedNumStr]} {
                    lappend currentUpdate $trimmedNumStr
                }
            }
            lappend updates $currentUpdate
        }
    }
    return [list $orderingRules $updates]
}

proc isCorrectlyOrdered {update rules} {
    set position [dict create]
    foreach num [lrange $update 0 end] {
        dict set position $num [lsearch $update $num]
    }

    foreach rule $rules {
        set x [lindex $rule 0]
        set y [lindex $rule 1]
        if {[dict exists $position $x] && [dict exists $position $y]} {
            set posX [dict get $position $x]
            set posY [dict get $position $y]
            if {$posX >= $posY} {
                return false
            }
        }
    }
    return true
}

proc topologicalSort {graph} {
    set inDegree [dict create]
    foreach node [dict keys $graph] {
        dict set inDegree $node 0
    }
    foreach node [dict keys $graph] {
        foreach neighbor [dict get $graph $node] {
            dict set inDegree $neighbor [expr {[dict get $inDegree $neighbor]} + 1]
        }
    }

    set queue {}
    foreach node [dict keys $inDegree] {
        if {[dict get $inDegree $node] == 0} {
            lappend queue $node
        }
    }

    set sorted {}
    while {[llength $queue] > 0} {
        set node [lindex $queue 0]
        set queue [lreplace $queue 0 0]
        lappend sorted $node
        if {[dict exists $graph $node]} {
             foreach neighbor [dict get $graph $node] {
                dict set inDegree $neighbor [expr {[dict get $inDegree $neighbor]} - 1]
                if {[dict get $inDegree $neighbor] == 0} {
                    lappend queue $neighbor
                }
            }
        }
    }
    return $sorted
}

proc sortUpdate {update rules} {
    set graph [dict create]
    set pagesInUpdate [dict create]
    foreach page $update {
        dict set graph $page {}
        dict set pagesInUpdate $page true
    }

    foreach rule $rules {
        set x [lindex $rule 0]
        set y [lindex $rule 1]
        if {[dict exists $pagesInUpdate $x] && [dict exists $pagesInUpdate $y]} {
            dict lappend graph $x $y
        }
    }

    set sorted [topologicalSort $graph]
    return [lreverse $sorted]
}

proc main {} {
    lassign [readInput input.txt] orderingRules updates
    set sum 0
    foreach update $updates {
        if {![isCorrectlyOrdered $update $orderingRules]} {
            set sortedUpdate [sortUpdate $update $orderingRules]
            set middleIndex [expr {[llength $sortedUpdate] / 2}]
            incr sum [lindex $sortedUpdate $middleIndex]
        }
    }
    puts $sum
}

main
