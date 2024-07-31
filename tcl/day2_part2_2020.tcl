set validCount 0
set file [open "input.txt" r]
while {[gets $file line] >= 0} {
    set i [string first ":" $line]
    if {$i != -1} {
        set policy [string range $line 0 $i-1]
        set password [string trim [string range $line $i+1 end]]
        regexp {(\d+)-(\d+) (\w)} $policy -> min max char
        set min [expr {$min - 1}]
        set max [expr {$max - 1}]
        set first [string index $password $min]
        set second [string index $password $max]
        if {($first eq $char) != ($second eq $char)} {
            incr validCount
        }
    }
}
close $file
puts $validCount