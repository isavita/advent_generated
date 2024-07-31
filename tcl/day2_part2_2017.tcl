set sum 0
set file [open "input.txt" r]
set data [read $file]
close $file

foreach line [split $data "\n"] {
    set nums [split $line]
    set n [llength $nums]
    for {set i 0} {$i < $n} {incr i} {
        set num1 [lindex $nums $i]
        for {set j 0} {$j < $n} {incr j} {
            if {$i != $j} {
                set num2 [lindex $nums $j]
                if {$num1 % $num2 == 0} {
                    set sum [expr {$sum + $num1 / $num2}]
                }
            }
        }
    }
}
puts $sum