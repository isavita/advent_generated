#!/usr/bin/env tclsh

set f [open "input.txt" r]
set lines {}
while {[gets $f line] >= 0} {
    regsub -all {\r|\n} $line "" line
    lappend lines $line
}
close $f
if {[llength $lines] == 0} {
    puts "Grand total: 0"
    exit
}
set maxw 0
foreach l $lines {
    if {[string length $l] > $maxw} {set maxw [string length $l]}
}
# pad lines to max width
set padded {}
foreach l $lines {
    set pad [string repeat " " [expr {$maxw - [string length $l]}]]
    lappend padded "${l}${pad}"
}
# find separator columns
set issep {}
for {set c 0} {$c < $maxw} {incr c} {
    set allspace 1
    foreach l $padded {
        if {[string index $l $c] ne " "} {set allspace 0; break}
    }
    lappend issep $allspace
}
set grandTotal 0
set inBlock 0
set start 0
for {set c 0} {$c < $maxw} {incr c} {
    if {[lindex $issep $c] == 0} {
        if {!$inBlock} {set inBlock 1; set start $c}
    } else {
        if {$inBlock} {
            # process block start..c-1
            set op +
            set nums {}
            for {set col $start} {$col < $c} {incr col} {
                set buf {}
                foreach l $padded {
                    set ch [string index $l $col]
                    if {[string is digit -strict $ch]} {
                        append buf $ch
                    } elseif {$ch eq "+" || $ch eq "*"} {
                        set op $ch
                    }
                }
                if {[string length $buf] > 0} {lappend nums $buf}
            }
            if {[llength $nums] > 0} {
                if {$op eq "*"} {
                    set blockRes 1
                    foreach n $nums {set blockRes [expr {$blockRes * $n}]}
                } else {
                    set blockRes 0
                    foreach n $nums {set blockRes [expr {$blockRes + $n}]}
                }
                set grandTotal [expr {$grandTotal + $blockRes}]
            }
            set inBlock 0
        }
    }
}
if {$inBlock} {
    set op +
    set nums {}
    for {set col $start} {$col < $maxw} {incr col} {
        set buf {}
        foreach l $padded {
            set ch [string index $l $col]
            if {[string is digit -strict $ch]} {
                append buf $ch
            } elseif {$ch eq "+" || $ch eq "*"} {
                set op $ch
            }
        }
        if {[string length $buf] > 0} {lappend nums $buf}
    }
    if {[llength $nums] > 0} {
        if {$op eq "*"} {
            set blockRes 1
            foreach n $nums {set blockRes [expr {$blockRes * $n}]}
        } else {
            set blockRes 0
            foreach n $nums {set blockRes [expr {$blockRes + $n}]}
        }
        set grandTotal [expr {$grandTotal + $blockRes}]
    }
}
puts "Grand total: $grandTotal"