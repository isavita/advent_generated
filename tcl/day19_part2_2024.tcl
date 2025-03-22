
proc count_ways {design patterns} {
    set n [string length $design]
    set dp [lrepeat [expr {$n + 1}] 0]
    lset dp 0 1
    for {set i 1} {$i <= $n} {incr i} {
        foreach p $patterns {
            set lp [string length $p]
            if {$i >= $lp && [string range $design [expr {$i - $lp}] [expr {$i - 1}]] eq $p} {
                lset dp $i [expr {[lindex $dp $i] + [lindex $dp [expr {$i - $lp}]]}]
            }
        }
    }
    return [lindex $dp $n]
}

proc solve {} {
    set file [open "input.txt" r]
    set line [gets $file]
    set patterns [split $line ,]
    set patterns [lmap p $patterns {string trim $p}]
    gets $file
    set total_ways 0
    while {[gets $file design] >= 0} {
        set design [string trim $design]
        set total_ways [expr {$total_ways + [count_ways $design $patterns]}]
    }
    close $file
    puts $total_ways
}

solve
