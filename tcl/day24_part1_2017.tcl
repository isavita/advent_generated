set maxStrength 0

proc findStrongestBridge {components used port strength} {
    global maxStrength
    if {$strength > $maxStrength} {
        set maxStrength $strength
    }
    for {set i 0} {$i < [llength $components]} {incr i} {
        set c [lindex $components $i]
        if {[lindex $used $i]} {continue}
        if {[lindex $c 0] eq $port || [lindex $c 1] eq $port} {
            set used [lreplace $used $i $i 1]
            set nextPort [lindex $c 0]
            if {[lindex $c 0] eq $port} {
                set nextPort [lindex $c 1]
            }
            findStrongestBridge $components $used $nextPort [expr {$strength + [lindex $c 0] + [lindex $c 1]}]
            set used [lreplace $used $i $i 0]
        }
    }
}

set file [open "input.txt" r]
set components {}
while {[gets $file line] >= 0} {
    set ports [split $line "/"]
    lappend components [list [lindex $ports 0] [lindex $ports 1]]
}
close $file

set used [list]
for {set i 0} {$i < [llength $components]} {incr i} {
    lappend used 0
}
findStrongestBridge $components $used 0 0

puts $maxStrength