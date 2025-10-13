
proc C {N} {
    array set L {}
    foreach K [split [string map {- ""} $N] {}] {
        incr L($K)
    }
    set S {}
    foreach K [array names L] {
        lappend S [list $L($K) $K]
    }
    set S [lsort -index 1 $S]
    set S [lsort -integer -decreasing -index 0 $S]
    set R ""
    foreach P [lrange $S 0 4] {
        append R [lindex $P 1]
    }
    return $R
}

set T 0
set F [open input.txt r]
while {[gets $F L] >= 0} {
    set P [split [string trim $L] -]
    set Last [lindex $P end]
    if {[regexp {(\d+)\[([a-z]+)\]} $Last M Id Chk]} {
        set N [join [lrange $P 0 end-1] "-"]
        if {[C $N] eq $Chk} {
            incr T $Id
        }
    }
}
close $F
puts $T
