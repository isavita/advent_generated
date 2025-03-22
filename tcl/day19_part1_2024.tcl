
proc can_make {design patterns} {
    set n [string length $design]
    set dp [lrepeat [expr {$n + 1}] 0]
    lset dp 0 1
    for {set i 1} {$i <= $n} {incr i} {
        foreach p $patterns {
            set lp [string length $p]
            if {$i >= $lp && [lindex $dp [expr {$i - $lp}]] && [string range $design [expr {$i - $lp}] [expr {$i - 1}]] eq $p} {
                lset dp $i 1
                break
            }
        }
    }
    return [lindex $dp $n]
}

proc main {} {
    set f [open "input.txt" r]
    set line [gets $f]
    set patterns [split [string trim $line] ","]
    set patterns [lmap p $patterns {string trim $p}]
    gets $f
    set count 0
    while {[gets $f line] >= 0} {
        set design [string trim $line]
        if {[can_make $design $patterns]} {
            incr count
        }
    }
    close $f
    puts $count
}

main
