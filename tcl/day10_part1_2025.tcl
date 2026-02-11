proc countSetBits {n} {
    set c 0
    set temp $n
    while {$temp > 0} {
        set temp [expr {$temp & ($temp - 1)}]
        incr c
    }
    return $c
}

proc solve {target buttons} {
    set R [llength $target]
    set C [llength $buttons]
    set rowMasks {}
    set rowTargets {}
    for {set r 0} {$r < $R} {incr r} {
        set m 0
        for {set c 0} {$c < $C} {incr c} {
            if {$r in [lindex $buttons $c]} {
                set m [expr {$m | (1 << $c)}]
            }
        }
        lappend rowMasks $m
        lappend rowTargets [lindex $target $r]
    }
    set pivotRow 0
    set pivots {}
    for {set c 0} {$c < $C && $pivotRow < $R} {incr c} {
        set sel -1
        for {set r $pivotRow} {$r < $R} {incr r} {
            if {([lindex $rowMasks $r] >> $c) & 1} {
                set sel $r
                break
            }
        }
        if {$sel == -1} continue
        set m1 [lindex $rowMasks $sel]
        set t1 [lindex $rowTargets $sel]
        lset rowMasks $sel [lindex $rowMasks $pivotRow]
        lset rowTargets $sel [lindex $rowTargets $pivotRow]
        lset rowMasks $pivotRow $m1
        lset rowTargets $pivotRow $t1
        for {set r 0} {$r < $R} {incr r} {
            if {$r != $pivotRow && (([lindex $rowMasks $r] >> $c) & 1)} {
                lset rowMasks $r [expr {[lindex $rowMasks $r] ^ [lindex $rowMasks $pivotRow]}]
                lset rowTargets $r [expr {[lindex $rowTargets $r] ^ [lindex $rowTargets $pivotRow]}]
            }
        }
        lappend pivots [list $c $pivotRow]
        incr pivotRow
    }
    for {set r $pivotRow} {$r < $R} {incr r} {
        if {[lindex $rowTargets $r] == 1} { return -1 }
    }
    set pivotColMask 0
    foreach p $pivots { set pivotColMask [expr {$pivotColMask | (1 << [lindex $p 0])}] }
    set freeVars {}
    for {set c 0} {$c < $C} {incr c} {
        if {!(([expr {$pivotColMask >> $c}]) & 1)} { lappend freeVars $c }
    }
    set target_mask 0
    for {set k 0} {$k < [llength $pivots]} {incr k} {
        if {[lindex $rowTargets [lindex $pivots $k 1]]} {
            set target_mask [expr {$target_mask | (1 << $k)}]
        }
    }
    set fv_effects {}
    foreach f $freeVars {
        set f_mask 0
        for {set k 0} {$k < [llength $pivots]} {incr k} {
            if {([lindex $rowMasks [lindex $pivots $k 1]] >> $f) & 1} {
                set f_mask [expr {$f_mask | (1 << $k)}]
            }
        }
        lappend fv_effects $f_mask
    }
    set masks [list $target_mask]
    set weights [list 0]
    set minWeight [countSetBits $target_mask]
    foreach f_mask $fv_effects {
        set added_masks {}
        set added_weights {}
        foreach m $masks w $weights {
            set nm [expr {$m ^ $f_mask}]
            set nw [expr {$w + 1}]
            lappend added_masks $nm
            lappend added_weights $nw
            set total [expr {$nw + [countSetBits $nm]}]
            if {$total < $minWeight} { set minWeight $total }
        }
        set masks [concat $masks $added_masks]
        set weights [concat $weights $added_weights]
    }
    return $minWeight
}

set totalPresses 0
if {![catch {set fp [open "input.txt" r]}]} {
    while {[gets $fp line] >= 0} {
        if {![regexp {\[([^\]]+)\](.*)} $line -> targetStr rest]} continue
        set target {}
        foreach ch [split $targetStr ""] { lappend target [expr {$ch eq "#" ? 1 : 0}] }
        set buttons {}
        foreach {match group} [regexp -all -inline {\(([^)]+)\)} $rest] {
            set b {}
            foreach val [split $group ","] { lappend b [string trim $val] }
            lappend buttons $b
        }
        set mw [solve $target $buttons]
        if {$mw != -1} { incr totalPresses $mw }
    }
    close $fp
}
puts $totalPresses