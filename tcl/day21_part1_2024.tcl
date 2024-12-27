
proc findPosition {mat ch} {
    for {set i 0} {$i < [llength $mat]} {incr i} {
        set row [lindex $mat $i]
        for {set j 0} {$j < [string length $row]} {incr j} {
            if {[string index $row $j] eq $ch} {
                return [list $i $j]
            }
        }
    }
    return [list -1 -1]
}

proc ok {mat st seq} {
    lassign $st i j
    foreach ch [split $seq ""] {
        if {[string index [lindex $mat $i] $j] eq " "} {
            return 0
        }
        switch $ch {
            "^" {incr i -1}
            "v" {incr i 1}
            "<" {incr j -1}
            ">" {incr j 1}
        }
        if {$i < 0 || $i >= [llength $mat] || $j < 0 || $j >= [string length [lindex $mat 0]]} {
            return 0
        }
    }
    return 1
}

proc generateMoves {position objective pad} {
    lassign $position posi posj
    lassign [findPosition $pad $objective] objPosi objPosj
    set ret ""
    if {$posj > $objPosj} {
        append ret [string repeat "<" [expr {$posj - $objPosj}]]
    }
    if {$posi > $objPosi} {
        append ret [string repeat "^" [expr {$posi - $objPosi}]]
    }
    if {$posi < $objPosi} {
        append ret [string repeat "v" [expr {$objPosi - $posi}]]
    }
    if {$posj < $objPosj} {
        append ret [string repeat ">" [expr {$objPosj - $posj}]]
    }
    if {![ok $pad $position $ret]} {
        set ret ""
        if {$posj < $objPosj} {
            append ret [string repeat ">" [expr {$objPosj - $posj}]]
        }
        if {$posi > $objPosi} {
            append ret [string repeat "^" [expr {$posi - $objPosi}]]
        }
        if {$posi < $objPosi} {
            append ret [string repeat "v" [expr {$objPosi - $posi}]]
        }
        if {$posj > $objPosj} {
            append ret [string repeat "<" [expr {$posj - $objPosj}]]
        }
    }
    return $ret
}

proc solve {code robots keyPad robotPad maxRobots} {
    if {$robots <= 0} {
        return [string length $code]
    }
    set ret 0
    set posi 3
    set posj 2
    if {$robots != $maxRobots} {
        set posi 0
    }
    for {set i 0} {$i < [string length $code]} {incr i} {
        set ch [string index $code $i]
        if {$robots == $maxRobots} {
            set moves [generateMoves [list $posi $posj] $ch $keyPad]
            lassign [findPosition $keyPad $ch] posi posj
        } else {
            set moves [generateMoves [list $posi $posj] $ch $robotPad]
            lassign [findPosition $robotPad $ch] posi posj
        }
        set ret [expr {$ret + [solve "${moves}A" [expr {$robots - 1}] $keyPad $robotPad $maxRobots]}]
    }
    return $ret
}

set maxRobots 3
set keyPad {
    "789"
    "456"
    "123"
    " 0A"
}
set robotPad {
    " ^A"
    "<v>"
}

set ret 0
set file [open "input.txt" r]
while {[gets $file line] >= 0} {
    set code [string trim $line]
    if {$code eq ""} {
        continue
    }
    set numericPart 0
    for {set i 0} {$i < [string length $code]} {incr i} {
        set char [string index $code $i]
        if {$char >= "0" && $char <= "9"} {
            set numericPart [expr {$numericPart * 10 + ($char - "0")}]
        }
    }
    set sv [solve $code $maxRobots $keyPad $robotPad $maxRobots]
    set ret [expr {$ret + $sv * $numericPart}]
}
close $file
puts $ret
