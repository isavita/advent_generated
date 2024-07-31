set input [open "input.txt" r]
set lines [split [read $input] "\n"]
close $input

set slopes {{1 1} {3 1} {5 1} {7 1} {1 2}}
set product 1

foreach slope $slopes {
    set treeCount 0
    set pos 0
    for {set i 0} {$i < [llength $lines]} {incr i [lindex $slope 1]} {
        if {[string index [lindex $lines $i] $pos] eq "#"} {
            incr treeCount
        }
        set pos [expr {($pos + [lindex $slope 0]) % [string length [lindex $lines $i]]}]
    }
    set product [expr {$product * $treeCount}]
}

puts $product