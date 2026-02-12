#!/usr/bin/env tclsh
set fp [open "input.txt" r]
set data [read $fp]
close $fp
set lines [split $data "\n"]
set nlines [llength $lines]
set num_monkeys 0
for {set idx 0} {$idx < $nlines} {incr idx} {
    set line [string trim [lindex $lines $idx]]
    if {$line eq ""} continue
    if {[regexp {^Monkey\s+\d+:} $line]} {
        set cur $num_monkeys
        incr num_monkeys
        incr idx
        set l [string trim [lindex $lines $idx]]
        if {[regexp {Starting items:\s*(.*)} $l -> items_str]} {
            set parts [split $items_str ","]
            set itemslist {}
            foreach p $parts {
                set p [string trim $p]
                if {$p ne ""} {lappend itemslist $p}
            }
            set items($cur) $itemslist
        } else {set items($cur) {}}
        incr idx
        set l [string trim [lindex $lines $idx]]
        if {[regexp {Operation:\s*new\s*=\s*old\s*([+*])\s*(\S+)} $l -> opt opvalstr]} {
            set op_type($cur) $opt
            if {$opvalstr eq "old"} {set op_val($cur) -1} else {set op_val($cur) $opvalstr}
        }
        incr idx
        set l [string trim [lindex $lines $idx]]
        if {[regexp {Test:\s*divisible\s*by\s*(\d+)} $l -> divv]} {set div($cur) $divv} else {set div($cur) 1}
        incr idx
        set l [string trim [lindex $lines $idx]]
        if {[regexp {If true:\s*throw to monkey\s*(\d+)} $l -> t]} {set next_true($cur) $t} else {set next_true($cur) 0}
        incr idx
        set l [string trim [lindex $lines $idx]]
        if {[regexp {If false:\s*throw to monkey\s*(\d+)} $l -> f]} {set next_false($cur) $f} else {set next_false($cur) 0}
        set inspections($cur) 0
    }
}
set total_div 1
for {set i 0} {$i < $num_monkeys} {incr i} {set total_div [expr {$total_div * $div($i)}]}
set ROUNDS 10000
for {set r 0} {$r < $ROUNDS} {incr r} {
    for {set i 0} {$i < $num_monkeys} {incr i} {
        set lst [array get items $i]
        if {[info exists items($i)]} {set curitems $items($i)} else {set curitems {}}
        if {[llength $curitems] == 0} continue
        foreach item $curitems {
            incr inspections($i)
            if {$op_val($i) == -1} {set operand $item} else {set operand $op_val($i)}
            if {$op_type($i) eq "+"} {set item [expr {$item + $operand}]} else {set item [expr {$item * $operand}]}
            set item [expr {$item % $total_div}]
            if {[expr {$item % $div($i)}] == 0} {set target $next_true($i)} else {set target $next_false($i)}
            lappend items($target) $item
        }
        set items($i) {}
    }
}
set vals {}
for {set i 0} {$i < $num_monkeys} {incr i} {lappend vals $inspections($i)}
set sorted [lsort -integer -decreasing $vals]
puts [expr {[lindex $sorted 0] * [lindex $sorted 1]}]