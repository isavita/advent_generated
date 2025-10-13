proc addr {R A B} { expr {[lindex $R $A] + [lindex $R $B]} }
proc addi {R A B} { expr {[lindex $R $A] + $B} }
proc mulr {R A B} { expr {[lindex $R $A] * [lindex $R $B]} }
proc muli {R A B} { expr {[lindex $R $A] * $B} }
proc banr {R A B} { expr {[lindex $R $A] & [lindex $R $B]} }
proc bani {R A B} { expr {[lindex $R $A] & $B} }
proc borr {R A B} { expr {[lindex $R $A] | [lindex $R $B]} }
proc bori {R A B} { expr {[lindex $R $A] | $B} }
proc setr {R A B} { lindex $R $A }
proc seti {R A B} { set A }
proc gtir {R A B} { expr {$A > [lindex $R $B]} }
proc gtri {R A B} { expr {[lindex $R $A] > $B} }
proc gtrr {R A B} { expr {[lindex $R $A] > [lindex $R $B]} }
proc eqir {R A B} { expr {$A == [lindex $R $B]} }
proc eqri {R A B} { expr {[lindex $R $A] == $B} }
proc eqrr {R A B} { expr {[lindex $R $A] == [lindex $R $B]} }

proc load_program {} {
    set f [open input.txt r]
    set l [split [read $f] \n]
    close $f
    set P {}
    set I 0
    foreach L $l {
        set L [string trim $L]
        if {$L eq ""} continue
        if {[string match "#ip *" $L]} {
            regexp {#ip ([0-9]+)} $L -> I
            continue
        }
        if {[regexp {^(\w+) (\d+) (\d+) (\d+)} $L -> O A B C]} {
            lappend P [list $O $A $B $C]
        }
    }
    return [list $I $P]
}

proc run_program {ipr p r mc} {
    set ip 0
    set c 0
    set R $r
    set L [llength $p]
    while {$ip >= 0 && $ip < $L} {
        if {$mc > 0 && $c >= $mc} break
        incr c
        set R [lreplace $R $ipr $ipr $ip]
        set I [lindex $p $ip]
        set O [lindex $I 0]
        set A [lindex $I 1]
        set B [lindex $I 2]
        set C [lindex $I 3]
        set res [eval $O [list $R] $A $B]
        set R [lreplace $R $C $C $res]
        set ip [expr {[lindex $R $ipr] + 1}]
    }
    return $R
}

proc max_value {R} {
    set m 0
    foreach v $R {
        if {$v > $m} {set m $v}
    }
    return $m
}

proc sum_factors {n} {
    set t 0
    for {set i 1} {$i * $i <= $n} {incr i} {
        if {$n % $i == 0} {
            incr t $i
            set j [expr {$n / $i}]
            if {$i != $j} {
                incr t $j
            }
        }
    }
    return $t
}

proc main {} {
    lassign [load_program] ipr P
    set R {1 0 0 0 0 0}
    set R [run_program $ipr $P $R 1000]
    set N [max_value $R]
    puts [sum_factors $N]
}

main