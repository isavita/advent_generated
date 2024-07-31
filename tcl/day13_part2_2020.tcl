proc readBusIDs {fileName} {
    set file [open $fileName r]
    set line1 [gets $file]
    set line2 [gets $file]
    close $file

    set ids {}
    set offsets {}
    set busData [split $line2 ","]
    for {set i 0} {$i < [llength $busData]} {incr i} {
        set bus [lindex $busData $i]
        if {$bus ne "x"} {
            lappend ids [expr {$bus}]
            lappend offsets $i
        }
    }
    return [list $ids $offsets]
}

proc extendedGCD {a b} {
    if {$a == 0} {
        return [list 0 1]
    }
    set vals [extendedGCD [expr {$b % $a}] $a]
    set x1 [lindex $vals 0]
    set y1 [lindex $vals 1]
    set x [expr {$y1 - int($b / $a) * $x1}]
    return [list $x $x1]
}

proc findEarliestTimestamp {ids offsets} {
    set N 1
    foreach id $ids {
        set N [expr {$N * $id}]
    }

    set result 0
    for {set i 0} {$i < [llength $ids]} {incr i} {
        set id [lindex $ids $i]
        set ni [expr {$N / $id}]
        set vals [extendedGCD $ni $id]
        set xi [lindex $vals 0]
        set offset [lindex $offsets $i]
        set result [expr {$result + (-$offset + $id) % $id * $xi * $ni}]
    }
    return [expr {$result % $N}]
}

set busData [readBusIDs "input.txt"]
set ids [lindex $busData 0]
set offsets [lindex $busData 1]
set timestamp [findEarliestTimestamp $ids $offsets]
puts $timestamp