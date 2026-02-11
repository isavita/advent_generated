proc parse {line} {
    regsub -all {[0-9]+} $line { {v &} } line
    set line [string map {\[ " { l " \] " } " , " "} $line]
    return $line
}

proc compare {left right} {
    set tagL [lindex $left 0]
    set tagR [lindex $right 0]
    if {$tagL eq "v" && $tagR eq "v"} {
        set vL [lindex $left 1]
        set vR [lindex $right 1]
        if {$vL < $vR} { return -1 }
        if {$vL > $vR} { return 1 }
        return 0
    }
    if {$tagL eq "v"} { return [compare [list l $left] $right] }
    if {$tagR eq "v"} { return [compare $left [list l $right]] }

    set itemsL [lrange $left 1 end]
    set itemsR [lrange $right 1 end]
    set lenL [llength $itemsL]
    set lenR [llength $itemsR]
    set n [expr {$lenL < $lenR ? $lenL : $lenR}]

    for {set i 0} {$i < $n} {incr i} {
        set res [compare [lindex $itemsL $i] [lindex $itemsR $i]]
        if {$res != 0} { return $res }
    }

    if {$lenL < $lenR} { return -1 }
    if {$lenL > $lenR} { return 1 }
    return 0
}

set fp [open "input.txt" r]
set data {}
while {[gets $fp line] >= 0} {
    set line [string trim $line]
    if {$line ne ""} { lappend data $line }
}
close $fp

set sum 0
set idx 1
for {set i 0} {$i < [llength $data]} {incr i 2} {
    set left [lindex [parse [lindex $data $i]] 0]
    set right [lindex [parse [lindex $data [expr {$i + 1}]]] 0]
    if {[compare $left $right] == -1} {
        incr sum $idx
    }
    incr idx
}
puts $sum