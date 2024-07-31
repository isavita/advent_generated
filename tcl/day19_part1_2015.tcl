set file [open "input.txt" r]
set replacements {}
set molecule ""

while {[gets $file line] >= 0} {
    if {$line eq ""} continue
    if {[regexp {(.+) => (.+)} $line -> from to]} {
        lappend replacements "$from $to"
    } else {
        set molecule $line
    }
}
close $file

array set molecules {}

foreach rep $replacements {
    set parts [split $rep]
    set from [lindex $parts 0]
    set to [lindex $parts 1]
    set len [string length $from]
    for {set i 0} {$i <= [string length $molecule] - $len} {incr i} {
        if {[string range $molecule $i [expr {$i + $len - 1}]] eq $from} {
            set newMolecule [string range $molecule 0 [expr {$i - 1}]]$to[string range $molecule [expr {$i + $len}] end]
            set molecules($newMolecule) ""
        }
    }
}

puts [llength [array names molecules]]