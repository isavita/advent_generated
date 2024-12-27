
proc getComboVal {op A B C} {
    switch $op {
        0 -
        1 -
        2 -
        3 {return $op}
        4 {return $A}
        5 {return $B}
        6 {return $C}
        default {error "invalid combo operand"}
    }
}

set f [open "input.txt" r]
set A 0
set B 0
set C 0
set program {}

while {[gets $f line] >= 0} {
    set line [string trim $line]
    if {$line eq ""} continue
    if {[string match "Register A:*" $line]} {
        set A [lindex [split $line ":"] 1]
        set A [string trim $A]
    } elseif {[string match "Register B:*" $line]} {
        set B [lindex [split $line ":"] 1]
        set B [string trim $B]
    } elseif {[string match "Register C:*" $line]} {
        set C [lindex [split $line ":"] 1]
        set C [string trim $C]
    } elseif {[string match "Program:*" $line]} {
        set pStr [lindex [split $line ":"] 1]
        set pStr [string trim $pStr]
        set program [split $pStr ","]
        set program [lmap n $program {string trim $n}]
    }
}
close $f

set outputVals {}
set ip 0
while {$ip < [llength $program]} {
    if {$ip+1 >= [llength $program]} break
    set opcode [lindex $program $ip]
    set operand [lindex $program [incr ip]]
    switch $opcode {
        0 {
            set den [getComboVal $operand $A $B $C]
            if {$den == 0} {
                set A 0
            } else {
                set A [expr {$A / (1 << $den)}]
            }
        }
        1 {
            set B [expr {$B ^ $operand}]
        }
        2 {
            set B [expr {[getComboVal $operand $A $B $C] % 8}]
        }
        3 {
            if {$A != 0} {
                set ip $operand
                incr ip -1
            }
        }
        4 {
            set B [expr {$B ^ $C}]
        }
        5 {
            lappend outputVals [expr {[getComboVal $operand $A $B $C] % 8}]
        }
        6 {
            set den [getComboVal $operand $A $B $C]
            set B [expr {$A / (1 << $den)}]
        }
        7 {
            set den [getComboVal $operand $A $B $C]
            set C [expr {$A / (1 << $den)}]
        }
        default {
            break
        }
    }
    incr ip
}

puts [join $outputVals ","]
