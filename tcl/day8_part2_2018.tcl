proc readInput {filename} {
    set file [open $filename r]
    set line [gets $file]
    close $file
    return [split $line]
}

proc parseTree {data index} {
    set childCount [lindex $data $index]
    set metaCount [lindex $data [expr {$index + 1}]]
    set index [expr {$index + 2}]
    
    set childValues {}
    for {set i 0} {$i < $childCount} {incr i} {
        set result [parseTree $data $index]
        lappend childValues [lindex $result 0]
        set index [lindex $result 1]
    }
    
    set value 0
    if {$childCount == 0} {
        for {set i 0} {$i < $metaCount} {incr i} {
            set value [expr {$value + [lindex $data $index]}]
            incr index
        }
    } else {
        for {set i 0} {$i < $metaCount} {incr i} {
            set metadata [lindex $data $index]
            if {$metadata <= $childCount && $metadata > 0} {
                set value [expr {$value + [lindex $childValues [expr {$metadata - 1}]]}]
            }
            incr index
        }
    }
    return [list $value $index]
}

set numbers [readInput "input.txt"]
set value [lindex [parseTree $numbers 0] 0]
puts $value