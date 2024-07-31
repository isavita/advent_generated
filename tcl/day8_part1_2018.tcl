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
    set sum 0

    for {set i 0} {$i < $childCount} {incr i} {
        set result [parseTree $data $index]
        set sum [expr {$sum + [lindex $result 0]}]
        set index [lindex $result 1]
    }

    for {set i 0} {$i < $metaCount} {incr i} {
        set sum [expr {$sum + [lindex $data $index]}]
        incr index
    }

    return [list $sum $index]
}

set numbers [readInput "input.txt"]
set sum [lindex [parseTree $numbers 0] 0]
puts $sum