
#!/usr/bin/env tclsh

set f [open "input.txt" r]
set pos 50
set cnt 0

while {[gets $f line] >= 0} {
    if {[string length $line] == 0} continue
    set dir [string index $line 0]
    set amt [string range $line 1 end]
    if {$dir eq "R"} {
        incr pos $amt
    } else {
        incr pos -$amt
    }
    set pos [expr {$pos % 100}]
    if {$pos < 0} {set pos [expr {$pos + 100}]}
    if {$pos == 0} {incr cnt}
}
close $f
puts $cnt
