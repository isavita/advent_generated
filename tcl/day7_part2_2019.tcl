
proc readInput {filename} {
    set f [open $filename r]
    set data [read $f]
    close $f
    set prog {}
    foreach n [split $data ,] {lappend prog [string trim $n]}
    return $prog
}

set mem0 [readInput input.txt]

# Intcode engine: run until output/halt; returns output or -halt-
proc run {startPtr} {
    global mem inputs outputs
    set ptr $startPtr
    while 1 {
        set op [lindex $mem $ptr]
        set opc [expr {$op % 100}]
        set m1 [expr {$op/100%10}]
        set m2 [expr {$op/1000%10}]
        set m3 [expr {$op/10000%10}]
        switch $opc {
            1 {
                set a [lindex $mem [expr {$ptr+1}]]
                set b [lindex $mem [expr {$ptr+2}]]
                set c [lindex $mem [expr {$ptr+3}]]
                set v1 [expr {$m1?$a:[lindex $mem $a]}]
                set v2 [expr {$m2?$b:[lindex $mem $b]}]
                lset mem $c [expr {$v1+$v2}]
                incr ptr 4
            }
            2 {
                set a [lindex $mem [expr {$ptr+1}]]
                set b [lindex $mem [expr {$ptr+2}]]
                set c [lindex $mem [expr {$ptr+3}]]
                set v1 [expr {$m1?$a:[lindex $mem $a]}]
                set v2 [expr {$m2?$b:[lindex $mem $b]}]
                lset mem $c [expr {$v1*$v2}]
                incr ptr 4
            }
            3 {
                if {![llength $inputs]} {return $ptr}
                set a [lindex $mem [expr {$ptr+1}]]
                lset mem $a [lindex $inputs 0]
                set inputs [lrange $inputs 1 end]
                incr ptr 2
            }
            4 {
                set a [lindex $mem [expr {$ptr+1}]]
                set val [expr {$m1?$a:[lindex $mem $a]}]
                lappend outputs $val
                incr ptr 2
                return $ptr
            }
            5 {
                set a [lindex $mem [expr {$ptr+1}]]
                set b [lindex $mem [expr {$ptr+2}]]
                set v1 [expr {$m1?$a:[lindex $mem $a]}]
                set v2 [expr {$m2?$b:[lindex $mem $b]}]
                set ptr [expr {$v1?$v2:$ptr+3}]
            }
            6 {
                set a [lindex $mem [expr {$ptr+1}]]
                set b [lindex $mem [expr {$ptr+2}]]
                set v1 [expr {$m1?$a:[lindex $mem $a]}]
                set v2 [expr {$m2?$b:[lindex $mem $b]}]
                set ptr [expr {$v1==0?$v2:$ptr+3}]
            }
            7 {
                set a [lindex $mem [expr {$ptr+1}]]
                set b [lindex $mem [expr {$ptr+2}]]
                set c [lindex $mem [expr {$ptr+3}]]
                set v1 [expr {$m1?$a:[lindex $mem $a]}]
                set v2 [expr {$m2?$b:[lindex $mem $b]}]
                lset mem $c [expr {$v1<$v2}]
                incr ptr 4
            }
            8 {
                set a [lindex $mem [expr {$ptr+1}]]
                set b [lindex $mem [expr {$ptr+2}]]
                set c [lindex $mem [expr {$ptr+3}]]
                set v1 [expr {$m1?$a:[lindex $mem $a]}]
                set v2 [expr {$m2?$b:[lindex $mem $b]}]
                lset mem $c [expr {$v1==$v2}]
                incr ptr 4
            }
            99 {return -halt-}
            default {error "bad op $opc at $ptr"}
        }
    }
}

# generate all permutations of a list
proc perms {lst} {
    if {[llength $lst]<=1} {return [list $lst]}
    set res {}
    for {set i 0} {$i<[llength $lst]} {incr i} {
        set rest [lreplace $lst $i $i]
        foreach p [perms $rest] {
            lappend res [linsert $p 0 [lindex $lst $i]]
        }
    }
    return $res
}

# Part 1
set best 0
foreach ph [perms {0 1 2 3 4}] {
    set inp 0
    foreach p $ph {
        set mem $mem0
        set inputs [list $p $inp]
        set outputs {}
        run 0
        set inp [lindex $outputs end]
    }
    if {$inp>$best} {set best $inp}
}
puts $best

# Part 2
set best 0
foreach ph [perms {5 6 7 8 9}] {
    array set amp {A,ptr 0  A,mem {}  B,ptr 0  B,mem {}  C,ptr 0  C,mem {}  D,ptr 0  D,mem {}  E,ptr 0  E,mem {}}
    foreach x {A B C D E} {
        set amp($x,mem) $mem0
        set amp($x,inputs) [list [lindex $ph [expr {[scan $x %c]-65}]]]
    }
    set inp 0
    set lastThruster 0
    while 1 {
        foreach x {A B C D E} {
            lappend amp($x,inputs) $inp
            set mem $amp($x,mem)
            set inputs $amp($x,inputs)
            set outputs {}
            set res [run $amp($x,ptr)]
            set amp($x,ptr) $res
            set amp($x,mem) $mem
            set amp($x,inputs) {}
            if {$res eq "-halt-"} {
                if {$x eq "E" && $lastThruster>$best} {set best $lastThruster}
                continue
            }
            set inp [lindex $outputs end]
            if {$x eq "E"} {set lastThruster $inp}
        }
        set any 0
        foreach x {A B C D E} {if {$amp($x,ptr) ne "-halt-"} {set any 1; break}}
        if {!$any} break
    }
}
puts $best
