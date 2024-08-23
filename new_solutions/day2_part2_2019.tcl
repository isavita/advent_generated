proc runProgram {program noun verb} {
    set program [split $program ","]
    set program [lmap x $program {expr {$x}}]
    lset program 1 $noun
    lset program 2 $verb
    set i 0
    while {$i < [llength $program]} {
        set opcode [lindex $program $i]
        if {$opcode == 99} {
            break
        }
        set pos1 [lindex $program [expr {$i + 1}]]
        set pos2 [lindex $program [expr {$i + 2}]]
        set pos3 [lindex $program [expr {$i + 3}]]
        if {$opcode == 1} {
            lset program $pos3 [expr {[lindex $program $pos1] + [lindex $program $pos2]}]
        } elseif {$opcode == 2} {
            lset program $pos3 [expr {[lindex $program $pos1] * [lindex $program $pos2]}]
        }
        set i [expr {$i + 4}]
    }
    return [lindex $program 0]
}

set input [read [open "input.txt"]]
set target 19690720

for {set noun 0} {$noun <= 99} {incr noun} {
    for {set verb 0} {$verb <= 99} {incr verb} {
        if {[runProgram $input $noun $verb] == $target} {
            puts [expr {100 * $noun + $verb}]
            return
        }
    }
}