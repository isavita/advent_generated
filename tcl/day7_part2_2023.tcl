proc get_type {cards} {
    set counts {}
    foreach c [split $cards ""] { dict incr counts $c }
    set j 0
    if {[dict exists $counts J]} {
        set j [dict get $counts J]
        dict unset counts J
    }
    set v [lsort -integer -decreasing [dict values $counts]]
    if {[llength $v] == 0} { set v 5 } else { lset v 0 [expr {[lindex $v 0] + $j}] }
    switch -exact -- $v {
        5 { return 6 }
        {4 1} { return 5 }
        {3 2} { return 4 }
        {3 1 1} { return 3 }
        {2 2 1} { return 2 }
        {2 1 1 1} { return 1 }
        default { return 0 }
    }
}

proc get_hex {cards} {
    return [string map {A E K D Q C J 1 T A} $cards]
}

proc compare {a b} {
    lassign $a t1 h1
    lassign $b t2 h2
    if {$t1 != $t2} { return [expr {$t1 - $t2}] }
    return [string compare $h1 $h2]
}

set f [open "input.txt" r]
set hands {}
while {[gets $f line] >= 0} {
    if {[string trim $line] eq ""} continue
    lassign $line c b
    lappend hands [list [get_type $c] [get_hex $c] $b]
}
close $f

set sorted [lsort -command compare $hands]
set total 0
set i 1
foreach h $sorted {
    lassign $h type hex bid
    set total [expr {$total + $bid * $i}]
    incr i
}
puts $total