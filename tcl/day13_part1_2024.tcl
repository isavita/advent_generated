
proc parse_val {s} {
    regsub {^\s*(X\+|Y\+|X=|Y=)} [string trim $s] {} s
    return [expr {$s}]
}

proc parse_val_prize {s} {
    regsub {^\s*(X=|Y=)} [string trim $s] {} s
    return [expr {$s}]
}

proc parse_line {s} {
    set parts [split [string trim $s] ,]
    set x [parse_val [lindex $parts 0]]
    set y [parse_val [lindex $parts 1]]
    return [list $x $y]
}

proc parse_prize {s} {
    set parts [split [string trim $s] ,]
    set x [parse_val_prize [lindex $parts 0]]
    set y [parse_val_prize [lindex $parts 1]]
    return [list $x $y]
}

proc parse_machine {lines} {
    set m [dict create]
    foreach l $lines {
        regsub {Button A:} $l {A:} l
        regsub {Button B:} $l {B:} l
        regsub {Prize:} $l {P:} l
        if {[string match "A:*" $l]} {
            set coords [parse_line [string range $l 2 end]]
            dict set m ax [lindex $coords 0]
            dict set m ay [lindex $coords 1]
        } elseif {[string match "B:*" $l]} {
            set coords [parse_line [string range $l 2 end]]
            dict set m bx [lindex $coords 0]
            dict set m by [lindex $coords 1]
        } elseif {[string match "P:*" $l]} {
            set coords [parse_prize [string range $l 2 end]]
            dict set m px [lindex $coords 0]
            dict set m py [lindex $coords 1]
        }
    }
    return $m
}

proc solve_machine {m} {
    set min_cost -1
    for {set a_count 0} {$a_count <= 100} {incr a_count} {
        for {set b_count 0} {$b_count <= 100} {incr b_count} {
            set x [expr {[dict get $m ax] * $a_count + [dict get $m bx] * $b_count}]
            set y [expr {[dict get $m ay] * $a_count + [dict get $m by] * $b_count}]
            if {$x == [dict get $m px] && $y == [dict get $m py]} {
                set cost [expr {$a_count * 3 + $b_count}]
                if {$min_cost == -1 || $cost < $min_cost} {
                    set min_cost $cost
                }
            }
        }
    }
    return $min_cost
}

proc main {} {
    set f [open "input.txt" r]
    set lines [split [read $f] \n]
    close $f

    set machines {}
    set current_machine {}
    foreach line $lines {
        set line [string trim $line]
        if {$line eq ""} {
            if {[llength $current_machine] > 0} {
                lappend machines [parse_machine $current_machine]
                set current_machine {}
            }
        } else {
            lappend current_machine $line
        }
    }
    if {[llength $current_machine] > 0} {
        lappend machines [parse_machine $current_machine]
    }

    set results {}
    foreach m $machines {
        set result [solve_machine $m]
        if {$result != -1} {
            lappend results $result
        }
    }

    if {[llength $results] == 0} {
        puts "0 0"
    } else {
        puts "[llength $results] [tcl::mathop::+ {*}$results]"
    }
}

main
