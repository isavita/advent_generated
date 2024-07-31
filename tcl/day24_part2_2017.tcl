set maxStrength 0
set maxLength 0

proc findStrongestLongestBridge {components used port strength length} {
    global maxStrength maxLength
    if {$length > $maxLength || ($length == $maxLength && $strength > $maxStrength)} {
        set maxStrength $strength
        set maxLength $length
    }
    for {set i 0} {$i < [llength $components]} {incr i} {
        if {[lindex $used $i]} continue
        set c [lindex $components $i]
        if {[lindex $c 0] == $port || [lindex $c 1] == $port} {
            set used [lreplace $used $i $i 1]
            set nextPort [lindex $c 0]
            if {[lindex $c 0] == $port} {
                set nextPort [lindex $c 1]
            }
            findStrongestLongestBridge $components $used $nextPort [expr {$strength + [lindex $c 0] + [lindex $c 1]}] [expr {$length + 1}]
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

findStrongestLongestBridge $components $used 0 0 0
puts $maxStrength