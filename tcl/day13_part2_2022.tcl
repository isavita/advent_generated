
proc compare {a b} {
    set int_a [string is integer -strict $a]
    set int_b [string is integer -strict $b]

    if {$int_a && $int_b} {
        if {$a < $b} {return -1}
        if {$a > $b} {return 1}
        return 0
    }

    if {$int_a} {set a [list $a]}
    if {$int_b} {set b [list $b]}

    set len_a [llength $a]
    set len_b [llength $b]

    for {set i 0} {$i < $len_a && $i < $len_b} {incr i} {
        set res [compare [lindex $a $i] [lindex $b $i]]
        if {$res != 0} {return $res}
    }

    return [expr {$len_a - $len_b}]
}

set packets {}
set f [open input.txt r]
while {[gets $f line] >= 0} {
    if {[string trim $line] eq ""} continue
    lappend packets [string map {[ \{ ] \} , " "} $line]
}
close $f

lappend packets {{2}} {{6}}
set sorted [lsort -command compare $packets]

set idx1 [expr {[lsearch -exact $sorted {{2}}] + 1}]
set idx2 [expr {[lsearch -exact $sorted {{6}}] + 1}]

puts [expr {$idx1 * $idx2}]
