proc compareFreeVars {a b} {
    global maxPresses
    set ma [lindex $maxPresses $a]
    set mb [lindex $maxPresses $b]
    if {$ma < $mb} {return -1}
    if {$ma > $mb} {return 1}
    return 0
}

proc computePivots {fVals} {
    global numButtons numCounters matrix pivotCol freeVars maxPresses
    set presses [lrepeat $numButtons 0]
    for {set i 0} {$i < [llength $freeVars]} {incr i} {
        lset presses [lindex $freeVars $i] [lindex $fVals $i]
    }
    for {set r [expr {$numCounters - 1}]} {$r >= 0} {incr r -1} {
        set col [lindex $pivotCol $r]
        if {$col < 0} {
            if {abs([lindex $matrix $r $numButtons]) > 1e-6} {return -1}
            continue
        }
        set val [lindex $matrix $r $numButtons]
        for {set c [expr {$col + 1}]} {$c < $numButtons} {incr c} {
            set m_rc [lindex $matrix $r $c]
            if {abs($m_rc) > 1e-9} {
                set val [expr {$val - $m_rc * [lindex $presses $c]}]
            }
        }
        set intVal [expr {int(round($val))}]
        if {abs($val - $intVal) > 1e-6 || $intVal < 0 || $intVal > [lindex $maxPresses $col]} {
            return -1
        }
        lset presses $col $intVal
    }
    set s 0
    foreach p $presses { incr s $p }
    return $s
}

proc enumerate {idx currentSum} {
    global numFree freeVars maxPresses bestResult freeValues
    if {$currentSum >= $bestResult} return
    if {$idx == $numFree} {
        set s [computePivots $freeValues]
        if {$s > 0 && $s < $bestResult} { set bestResult $s }
        return
    }
    set fv [lindex $freeVars $idx]
    set limit [lindex $maxPresses $fv]
    for {set v 0} {$v <= $limit} {incr v} {
        lset freeValues $idx $v
        enumerate [expr {$idx + 1}] [expr {$currentSum + $v}]
    }
}

proc solve {btns tgts} {
    global buttons targets numCounters numButtons matrix pivotCol freeVars maxPresses numFree bestResult freeValues
    set buttons $btns
    set targets $tgts
    set numCounters [llength $targets]
    set numButtons [llength $buttons]
    set matrix {}
    for {set i 0} {$i < $numCounters} {incr i} {
        lappend matrix [lrepeat [expr {$numButtons + 1}] 0.0]
    }
    for {set j 0} {$j < $numCounters} {incr j} {
        lset matrix $j $numButtons [expr {double([lindex $targets $j])}]
    }
    for {set i 0} {$i < $numButtons} {incr i} {
        foreach c [lindex $buttons $i] {
            if {$c < $numCounters} { lset matrix $c $i 1.0 }
        }
    }
    set row 0
    set pivotCol [lrepeat $numCounters -1]
    for {set col 0} {$col < $numButtons && $row < $numCounters} {incr col} {
        set maxRow $row
        for {set r [expr {$row + 1}]} {$r < $numCounters} {incr r} {
            if {abs([lindex $matrix $r $col]) > abs([lindex $matrix $maxRow $col])} { set maxRow $r }
        }
        if {abs([lindex $matrix $maxRow $col]) < 1e-9} continue
        set tmp [lindex $matrix $row]
        lset matrix $row [lindex $matrix $maxRow]
        lset matrix $maxRow $tmp
        set scale [lindex $matrix $row $col]
        for {set c $col} {$c <= $numButtons} {incr c} {
            lset matrix $row $c [expr {[lindex $matrix $row $c] / $scale}]
        }
        for {set r 0} {$r < $numCounters} {incr r} {
            if {$r != $row} {
                set factor [lindex $matrix $r $col]
                if {abs($factor) > 1e-9} {
                    for {set c $col} {$c <= $numButtons} {incr c} {
                        set m_row_c [lindex $matrix $row $c]
                        set m_r_c [lindex $matrix $r $c]
                        lset matrix $r $c [expr {$m_r_c - $factor * $m_row_c}]
                    }
                }
            }
        }
        lset pivotCol $row $col
        incr row
    }
    set rank $row
    for {set r $rank} {$r < $numCounters} {incr r} {
        if {abs([lindex $matrix $r $numButtons]) > 1e-6} { return -1 }
    }
    set isPivot [lrepeat $numButtons 0]
    for {set r 0} {$r < $rank} {incr r} {
        set c [lindex $pivotCol $r]
        if {$c != -1} { lset isPivot $c 1 }
    }
    set freeVars {}
    for {set i 0} {$i < $numButtons} {incr i} {
        if {![lindex $isPivot $i]} { lappend freeVars $i }
    }
    set maxPresses {}
    for {set i 0} {$i < $numButtons} {incr i} {
        set m 1000000000
        foreach c [lindex $buttons $i] {
            if {$c < $numCounters} {
                set t_val [lindex $targets $c]
                if {$t_val < $m} {set m $t_val}
            }
        }
        if {$m == 1000000000} {set m 0}
        lappend maxPresses $m
    }
    set freeVars [lsort -command compareFreeVars $freeVars]
    set numFree [llength $freeVars]
    set freeValues [lrepeat $numFree 0]
    set bestResult 1000000000
    enumerate 0 0
    if {$bestResult == 1000000000} { return -1 }
    return $bestResult
}

set total_all 0
if {[file exists "input.txt"]} {
    set fp [open "input.txt" r]
    while {[gets $fp line] >= 0} {
        set line [string trim $line]
        if {$line eq ""} continue
        set parts [string map {"(" " {" ")" "} " "{" " {" "}" "}" "," " "} $line]
        set targets [lindex $parts end]
        set buttons [lrange $parts 0 end-1]
        set res [solve $buttons $targets]
        if {$res > 0} { set total_all [expr {$total_all + $res}] }
    }
    close $fp
}
puts $total_all