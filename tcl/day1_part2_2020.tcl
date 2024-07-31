set file [open "input.txt" r]
set expenses [split [read $file] "\n"]
close $file

set target 2020
set result 0

for {set i 0} {$i < [llength $expenses]} {incr i} {
    set a [lindex $expenses $i]
    for {set j [expr {$i + 1}]} {$j < [llength $expenses]} {incr j} {
        set b [lindex $expenses $j]
        for {set k [expr {$j + 1}]} {$k < [llength $expenses]} {incr k} {
            set c [lindex $expenses $k]
            if {[expr {$a + $b + $c}] == $target} {
                set result [expr {$a * $b * $c}]
                puts $result
                return
            }
        }
    }
}