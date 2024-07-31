proc readNodes {filename} {
    set nodes {}
    set file [open $filename r]
    set regex {node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%}
    while {[gets $file line] >= 0} {
        if {[regexp $regex $line match used avail]} {
            lappend nodes [list $used $avail]
        }
    }
    close $file
    return $nodes
}

proc countViablePairs {nodes} {
    set count 0
    set n [llength $nodes]
    for {set i 0} {$i < $n} {incr i} {
        set a [lindex $nodes $i]
        set used [lindex $a 0]
        set avail [lindex $a 1]
        for {set j 0} {$j < $n} {incr j} {
            if {$i != $j} {
                set b [lindex $nodes $j]
                set bAvail [lindex $b 1]
                if {$used > 0 && $used <= $bAvail} {
                    incr count
                }
            }
        }
    }
    return $count
}

set nodes [readNodes "input.txt"]
set viablePairs [countViablePairs $nodes]
puts $viablePairs