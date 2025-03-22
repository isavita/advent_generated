
proc solve {} {
    set graph {}
    set file [open "input.txt" r]
    while {[gets $file line] != -1} {
        lassign [split $line "-"] a b
        dict lappend graph $a $b
        dict lappend graph $b $a
    }
    close $file

    set computers [dict keys $graph]
    set count 0
    set seen {}

    for {set i 0} {$i < [llength $computers]} {incr i} {
        for {set j [expr {$i + 1}]} {$j < [llength $computers]} {incr j} {
            for {set k [expr {$j + 1}]} {$k < [llength $computers]} {incr k} {
                set c1 [lindex $computers $i]
                set c2 [lindex $computers $j]
                set c3 [lindex $computers $k]

                if {[lsearch -exact [dict get $graph $c1] $c2] >= 0 && \
                    [lsearch -exact [dict get $graph $c2] $c3] >= 0 && \
                    [lsearch -exact [dict get $graph $c1] $c3] >= 0} {
                    if {[string match t* $c1] || [string match t* $c2] || [string match t* $c3]} {
                        set triplet [lsort [list $c1 $c2 $c3]]
                        set triplet_str [join $triplet ","]
                        if {![dict exists $seen $triplet_str]} {
                            dict set seen $triplet_str 1
                            incr count
                        }
                    }
                }
            }
        }
    }
    puts $count
}

solve
