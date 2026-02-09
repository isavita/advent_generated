
set fp [open "input.txt" r]
set num_stacks 0
array set stacks {}
while {[gets $fp line] >= 0} {
    if {[string length $line] > 1 && [string index $line 1] eq "1"} {break}
    set len [string length $line]
    for {set i 1} {$i < $len} {incr i 4} {
        set ch [string index $line $i]
        if {[string is alpha $ch]} {
            set idx [expr {$i/4}]
            if {$idx >= $num_stacks} {set num_stacks [expr {$idx+1}]}
            if {[info exists stacks($idx)]} {
                lappend stacks($idx) $ch
            } else {
                set stacks($idx) [list $ch]
            }
        }
    }
}
for {set i 0} {$i < $num_stacks} {incr i} {
    if {[info exists stacks($i)]} {
        set stacks($i) [lreverse $stacks($i)]
    } else {
        set stacks($i) {}
    }
}
while {[gets $fp line] >= 0} {
    if {[string match "move*" $line]} {
        regexp {move (\d+) from (\d+) to (\d+)} $line -> n from to
        set fromIdx [expr {$from-1}]
        set toIdx   [expr {$to-1}]
        set src $stacks($fromIdx)
        set moved [lrange $src end-[expr {$n-1}] end]
        set src [lrange $src 0 end-$n]
        set dst $stacks($toIdx)
        set stacks($fromIdx) $src
        set stacks($toIdx)   [concat $dst $moved]
    }
}
close $fp
set result ""
for {set i 0} {$i < $num_stacks} {incr i} {
    if {[llength $stacks($i)]} {
        append result [lindex $stacks($i) end]
    }
}
puts $result
