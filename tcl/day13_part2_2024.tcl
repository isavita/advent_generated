
proc parseLine {s} {
    set parts [split [string trim $s] ","]
    return [list [parseVal [lindex $parts 0]] [parseVal [lindex $parts 1]]]
}
proc parsePrize {s} {
    set parts [split [string trim $s] ","]
    return [list [parseValPrize [lindex $parts 0]] [parseValPrize [lindex $parts 1]]]
}
proc parseVal {s} {
    set s [string trim $s]
    foreach p {"X+" "Y+" "X=" "Y="} {
        if {[string match "$p*" $s]} {
            set s [string range $s [string length $p] end]
        }
    }
    return [scan $s %ld]
}
proc parseValPrize {s} {
    set s [string trim $s]
    foreach p {"X=" "Y="} {
        if {[string match "$p*" $s]} {
            set s [string range $s [string length $p] end]
        }
    }
    return [scan $s %ld]
}
proc solveMachine {m} {
    lassign $m ax ay bx by px py
    set D [expr {$ax*$by - $ay*$bx}]
    if {$D == 0} {return -1}
    set numA [expr {$px*$by - $py*$bx}]
    set numB [expr {-$px*$ay + $py*$ax}]
    if {[expr {$numA % $D}] != 0 || [expr {$numB % $D}] != 0} {return -1}
    set a [expr {$numA / $D}]
    set b [expr {$numB / $D}]
    if {$a < 0 || $b < 0} {return -1}
    return [expr {3*$a + $b}]
}
proc readInput {filename} {
    set f [open $filename r]
    set machines {}
    set lines {}
    while {[gets $f line] >= 0} {
        set line [string trim $line]
        if {$line eq ""} {
            if {[llength $lines] > 0} {
                lappend machines [parseMachine $lines]
                set lines {}
            }
        } else {
            lappend lines $line
        }
    }
    if {[llength $lines] > 0} {
        lappend machines [parseMachine $lines]
    }
    close $f
    return $machines
}
proc parseMachine {lines} {
    set m {}
    foreach l $lines {
        set l [string map {"Button A:" "A:" "Button B:" "B:" "Prize:" "P:"} $l]
        if {[string match "A:*" $l]} {
            lassign [parseLine [string range $l 2 end]] ax ay
            set m [concat $m $ax $ay]
        } elseif {[string match "B:*" $l]} {
            lassign [parseLine [string range $l 2 end]] bx by
            set m [concat $m $bx $by]
        } elseif {[string match "P:*" $l]} {
            lassign [parsePrize [string range $l 2 end]] px py
            set m [concat $m $px $py]
        }
    }
    return $m
}
set offset 10000000000000
set machines [readInput "input.txt"]
set results {}
foreach m $machines {
    lassign $m ax ay bx by px py
    set px [expr {$px + $offset}]
    set py [expr {$py + $offset}]
    set cost [solveMachine [list $ax $ay $bx $by $px $py]]
    if {$cost >= 0} {
        lappend results $cost
    }
}
if {[llength $results] == 0} {
    puts "0 0"
} else {
    set count [llength $results]
    set sum 0
    foreach c $results {
        set sum [expr {$sum + $c}]
    }
    puts "$count $sum"
}
