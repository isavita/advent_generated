
proc calc {c a} {
    global R I S
    if {$c eq "ORE"} {return $a}
    if {$S($c) >= $a} {
        incr S($c) -$a
        return 0
    }
    set n [expr {$a - $S($c)}]
    set S($c) 0
    set q $R($c)
    set t [expr {($n + $q - 1) / $q}]
    set o 0
    foreach i $I($c) {
        lassign $i iq in
        incr o [calc $in [expr {$iq * $t}]]
    }
    incr S($c) [expr {$t * $q - $n}]
    return $o
}

set f [open "input.txt"]
while {[gets $f l] >= 0} {
    set i [string first " => " $l]
    if {$i == -1} continue
    scan [string range $l $i+4 end] "%d %s" q n
    set R($n) $q
    set S($n) 0
    set I($n) {}
    foreach g [split [string range $l 0 $i-1] ","] {
        scan $g "%d %s" gq gn
        lappend I($n) [list $gq $gn]
    }
}
close $f

puts [calc FUEL 1]
