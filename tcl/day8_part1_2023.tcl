proc main {} {
    set file [open "input.txt" r]
    set instructions [string trim [gets $file]]
    array set nodes {}

    while {[gets $file line] >= 0} {
        if {[regexp {(\w+) = \((\w+), (\w+)\)} $line match node left right]} {
            set nodes($node) [list $left $right]
        }
    }
    close $file

    set current "AAA"
    set step 0
    set i 0

    while {$current ne "ZZZ"} {
        set dir [string index $instructions $i]
        set i [expr {($i + 1) % [string length $instructions]}]
        set next [lindex $nodes($current) [expr {$dir eq "L" ? 0 : 1}]]
        set current $next
        incr step
    }

    puts $step
}

main