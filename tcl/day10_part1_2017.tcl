set file [open "input.txt" r]
set lengths [split [gets $file] ","]
close $file

set list [list]
for {set i 0} {$i < 256} {incr i} {
    lappend list $i
}

set currentPosition 0
set skipSize 0

foreach length $lengths {
    set length [expr {$length + 0}]
    for {set i 0} {$i < $length / 2} {incr i} {
        set start [expr {($currentPosition + $i) % 256}]
        set end [expr {($currentPosition + $length - 1 - $i) % 256}]
        set temp [lindex $list $start]
        set list [lreplace $list $start $start [lindex $list $end]]
        set list [lreplace $list $end $end $temp]
    }
    set currentPosition [expr {($currentPosition + $length + $skipSize) % 256}]
    incr skipSize
}

set result [expr {[lindex $list 0] * [lindex $list 1]}]
puts $result