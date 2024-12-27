
proc is_safe {report} {
    set levels [split $report]
    set len [llength $levels]
    if {$len <= 1} {
        return 1
    }

    set increasing [expr {[lindex $levels 1] > [lindex $levels 0]}]
    
    for {set i 0} {$i < $len - 1} {incr i} {
        set current [lindex $levels $i]
        set next [lindex $levels [expr {$i + 1}]]
        set diff [expr {$next - $current}]

        if {$diff == 0 || abs($diff) > 3} {
            return 0
        }

        if {$i > 0} {
            set prev [lindex $levels [expr {$i - 1}]]
            if {($next > $current && $current < $prev) || ($next < $current && $current > $prev)} {
                return 0
            }
        }
    }
    return 1
}

set safe_count 0
if {[file exists "input.txt"]} {
    set file [open "input.txt" r]
    while {[gets $file line] != -1} {
        if {[is_safe $line]} {
            incr safe_count
        }
    }
    close $file
}

puts $safe_count
