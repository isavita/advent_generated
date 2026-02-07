#!/usr/bin/env tclsh
set f [open "input.txt"]
set lines {}
set maxw 0
while {[gets $f line] >= 0} {
    set line [string trimright $line "\r"]
    lappend lines $line
    set l [string length $line]
    if {$l > $maxw} {set maxw $l}
}
close $f
set nlines [llength $lines]
set grand 0
proc is_sep {col lines nlines} {
    for {set i 0} {$i < $nlines} {incr i} {
        set line [lindex $lines $i]
        if {$col < [string length $line] && ![string is space -strict [string index $line $col]]} {
            return 0
        }
    }
    return 1
}
proc process_block {sc ec lines nlines grandVar} {
    upvar $grandVar grand
    set nums {}
    set op 0
    for {set i 0} {$i < $nlines} {incr i} {
        set line [lindex $lines $i]
        set e $ec
        if {$e >= [string length $line]} {set e [expr {[string length $line] - 1}]}
        if {$sc > [string length $line] - 1} continue
        set seg [string range $line $sc $e]
        set seg [string trim $seg]
        if {$seg eq ""} continue
        if {$seg eq "+"} {set op 1} elseif {$seg eq "*"} {set op 2} else {lappend nums $seg}
    }
    if {[llength $nums] == 0} return
    if {$op == 1} {
        set acc 0
        foreach n $nums {set acc [expr {$acc + $n}]}
    } elseif {$op == 2} {
        set acc 1
        foreach n $nums {set acc [expr {$acc * $n}]}
    } else {
        set acc [lindex $nums 0]
    }
    set grand [expr {$grand + $acc}]
}
set inb 0
set sc 0
for {set x 0} {$x < $maxw} {incr x} {
    if {[is_sep $x $lines $nlines] == 0} {
        if {!$inb} {set inb 1; set sc $x}
    } else {
        if {$inb} {process_block $sc [expr {$x-1}] $lines $nlines grand; set inb 0}
    }
}
if {$inb} {process_block $sc [expr {$maxw-1}] $lines $nlines grand}
puts "Grand total: $grand"