
proc compute_operand {val a b c} {
    if {$val < 4} {
        return $val
    } elseif {$val == 4} {
        return $a
    } elseif {$val == 5} {
        return $b
    } elseif {$val == 6} {
        return $c
    } else {
        return -1 ;# Error condition to check 
    }
}

proc simulate_computer {program} {
    upvar $program prog
    set outs {}
    set a [dict get $prog a]
    set b [dict get $prog b]
    set c [dict get $prog c]
    set input_program [dict get $prog program]
    set i 0
    while {$i < [llength $input_program]} {
        set cmd [lindex $input_program $i]
        incr i
        if {$cmd == 0} {
            set a [expr {$a >> [compute_operand [lindex $input_program $i] $a $b $c]}]
        } elseif {$cmd == 1} {
            set b [expr {$b ^ [lindex $input_program $i]}]
        } elseif {$cmd == 2} {
            set b [expr {[compute_operand [lindex $input_program $i] $a $b $c] % 8}]
        } elseif {$cmd == 3} {
            if {$a != 0} {
                set i [expr {[lindex $input_program $i] - 1}]
            } else {
                incr i
            }
        } elseif {$cmd == 4} {
            set b [expr {$b ^ $c}]
        } elseif {$cmd == 5} {
            lappend outs [expr {[compute_operand [lindex $input_program $i] $a $b $c] % 8}]
        } elseif {$cmd == 6} {
            set b [expr {$a >> [compute_operand [lindex $input_program $i] $a $b $c]}]
        } elseif {$cmd == 7} {
            set c [expr {$a >> [compute_operand [lindex $input_program $i] $a $b $c]}]
        } else {
            return -1 ;#Invalid OP code
        }
        if {$i < [llength $input_program]} {
            incr i
        }
    }
    return $outs
}

proc check {program} {
    set valids {}
    set stack [list [list 0 0]]
    set seen {}

    while {[llength $stack] > 0} {
        set current [lindex $stack 0]
        set stack [lrange $stack 1 end]
        set depth [lindex $current 0]
        set score [lindex $current 1]
        set key "$depth $score"

        if {[dict exists $seen $key]} {
            continue
        }
        dict set seen $key 1

        if {$depth == [llength [dict get $program program]]} {
            lappend valids $score
        } else {
            for {set i 0} {$i < 8} {incr i} {
                set new_score [expr {$i + 8 * $score}]
                set test_program [dict create a $new_score b [dict get $program b] c [dict get $program c] program [dict get $program program]]
                set result [simulate_computer test_program]
                if {[llength $result] > 0 && [lindex $result 0] == [lindex [dict get $program program] [expr {[llength [dict get $program program]] - 1 - $depth}]]} {
                    lappend stack [list [expr {$depth + 1}] $new_score]
                }
            }
        }
    }
    return $valids
}

proc main {} {
    set filename "input.txt"
    set file [open $filename r]
    set a 0
    set b 0
    set c 0
    set program {}

    while {[gets $file line] >= 0} {
        set line [string trim $line]
        if {[string match "Register A:*" $line]} {
            set a [string trim [lindex [split $line ":"] 1]]
        } elseif {[string match "Register B:*" $line]} {
            set b [string trim [lindex [split $line ":"] 1]]
        } elseif {[string match "Register C:*" $line]} {
            set c [string trim [lindex [split $line ":"] 1]]
        } elseif {[string match "Program:*" $line]} {
            set program [split [string trim [lindex [split $line ":"] 1]] ","]
            foreach {i} $program {
                set program_int [lappend program_int [string trim $i]]
            }
            set program $program_int
        }
    }
    close $file

    set p [dict create a $a b $b c $c program $program]
    set valid_values [check $p]
    set min_val [lindex $valid_values 0]
    foreach val $valid_values {
        if {$val < $min_val} {
            set min_val $val
        }
    }
    puts $min_val
}

main
