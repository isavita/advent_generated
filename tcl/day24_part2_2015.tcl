
proc main {} {
    set fp [open "input.txt" r]
    set packages {}
    set totalWeight 0
    while {[gets $fp line] != -1} {
        set val [string trim $line]
        if {$val ne ""} {
            lappend packages $val
            incr totalWeight $val
        }
    }
    close $fp

    set targetWeight [expr {$totalWeight / 4}]
    
    # Optimization: Sort packages descending to find valid groups faster
    set packages [lsort -integer -decreasing $packages]
    set n [llength $packages]
    
    set bestQE -1
    
    # Iterate by group size (k) to find the smallest group first
    for {set k 1} {$k <= $n} {incr k} {
        set minQEForK -1
        
        foreach indices [combinations $n $k] {
            set groupWeight 0
            set qe 1
            foreach i $indices {
                set val [lindex $packages $i]
                incr groupWeight $val
                set qe [expr {$qe * $val}]
            }
            
            if {$groupWeight == $targetWeight} {
                set mask 0
                foreach i $indices { incr mask [expr {1 << $i}] }
                
                set remaining {}
                for {set i 0} {$i < $n} {incr i} {
                    if {!($mask & (1 << $i))} { lappend remaining [lindex $packages $i] }
                }
                
                if {[canSplit $remaining $targetWeight]} {
                    if {$minQEForK == -1 || $qe < $minQEForK} {
                        set minQEForK $qe
                    }
                }
            }
        }
        
        if {$minQEForK != -1} {
            puts $minQEForK
            return
        }
    }
}

proc combinations {n k} {
    if {$k == 0} {return {{}}}
    set res {}
    for {set i 0} {$i <= $n - $k} {incr i} {
        foreach sub [combinations [expr {$n - $i - 1}] [expr {$k - 1}]] {
            set subIdx {}
            foreach s $sub { lappend subIdx [expr {$s + $i + 1}] }
            lappend res [list $i {*}$subIdx]
        }
    }
    return $res
}

proc canSplit {packages target} {
    set n [llength $packages]
    set limit [expr {1 << $n}]
    
    for {set m1 1} {$m1 < $limit} {incr m1} {
        set w1 0
        for {set i 0} {$i < $n} {incr i} {
            if {$m1 & (1 << $i)} { incr w1 [lindex $packages $i] }
        }
        if {$w1 == $target} {
            set rem {}
            for {set i 0} {$i < $n} {incr i} {
                if {!($m1 & (1 << $i))} { lappend rem [lindex $packages $i] }
            }
            
            set m2len [llength $rem]
            set limit2 [expr {1 << $m2len}]
            for {set m2 1} {$m2 < $limit2} {incr m2} {
                set w2 0
                for {set i 0} {$i < $m2len} {incr i} {
                    if {$m2 & (1 << $i)} { incr w2 [lindex $rem $i] }
                }
                if {$w2 == $target} { return 1 }
            }
        }
    }
    return 0
}

main
