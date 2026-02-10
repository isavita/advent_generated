
proc run {op reg A B C} {
    set vA [lindex $reg $A]
    set vB [lindex $reg $B]
    switch $op {
        addr { lset reg $C [expr {$vA + $vB}] }
        addi { lset reg $C [expr {$vA + $B}] }
        mulr { lset reg $C [expr {$vA * $vB}] }
        muli { lset reg $C [expr {$vA * $B}] }
        banr { lset reg $C [expr {$vA & $vB}] }
        bani { lset reg $C [expr {$vA & $B}] }
        borr { lset reg $C [expr {$vA | $vB}] }
        bori { lset reg $C [expr {$vA | $B}] }
        setr { lset reg $C $vA }
        seti { lset reg $C $A }
        gtir { lset reg $C [expr {$A > $vB ? 1 : 0}] }
        gtri { lset reg $C [expr {$vA > $B ? 1 : 0}] }
        gtrr { lset reg $C [expr {$vA > $vB ? 1 : 0}] }
        eqir { lset reg $C [expr {$A == $vB ? 1 : 0}] }
        eqri { lset reg $C [expr {$vA == $B ? 1 : 0}] }
        eqrr { lset reg $C [expr {$vA == $vB ? 1 : 0}] }
    }
    return $reg
}

set ops {addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr}
set f [open "input.txt" r]
set data [split [read $f] \n]
close $f

set samples {}
set program {}
set mode samples

foreach line $data {
    if {[string match "Before*" $line]} {
        regexp {\[(\d+), (\d+), (\d+), (\d+)\]} $line -> b0 b1 b2 b3
        set before [list $b0 $b1 $b2 $b3]
    } elseif {[string match "After*" $line]} {
        regexp {\[(\d+), (\d+), (\d+), (\d+)\]} $line -> a0 a1 a2 a3
        set after [list $a0 $a1 $a2 $a3]
        lappend samples [list $before $instr $after]
        set mode samples
    } elseif {[regexp {^(\d+) (\d+) (\d+) (\d+)$} $line -> o A B C]} {
        set instr [list $o $A $B $C]
        if {$mode eq "samples"} {
             set mode instr
        } else {
             lappend program $instr
        }
    }
}

set p1 0
foreach s $samples {
    lassign $s before instr after
    lassign $instr opNum A B C
    set count 0
    foreach op $ops {
        if {[run $op $before $A $B $C] eq $after} { incr count }
    }
    if {$count >= 3} { incr p1 }
}
puts $p1

array set map {}
for {set i 0} {$i < 16} {incr i} { set possible($i) $ops }

foreach s $samples {
    lassign $s before instr after
    lassign $instr opNum A B C
    set newPoss {}
    foreach op $possible($opNum) {
        if {[run $op $before $A $B $C] eq $after} { lappend newPoss $op }
    }
    set possible($opNum) $newPoss
}

while {[array size map] < 16} {
    for {set i 0} {$i < 16} {incr i} {
        if {[info exists map($i)]} continue
        if {[llength $possible($i)] == 1} {
            set val [lindex $possible($i) 0]
            set map($i) $val
            for {set j 0} {$j < 16} {incr j} {
                if {$i != $j} {
                    set idx [lsearch $possible($j) $val]
                    if {$idx != -1} { set possible($j) [lreplace $possible($j) $idx $idx] }
                }
            }
        }
    }
}

set regs {0 0 0 0}
foreach instr $program {
    lassign $instr opNum A B C
    set regs [run $map($opNum) $regs $A $B $C]
}
puts [lindex $regs 0]
