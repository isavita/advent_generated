proc hasABBA {s} {
    set len [string length $s]
    for {set i 0} {$i <= $len - 4} {incr i} {
        set a [string index $s $i]
        set b [string index $s [expr {$i + 1}]]
        if {$a ne $b} {
            if {[string index $s [expr {$i + 2}]] eq $b && [string index $s [expr {$i + 3}]] eq $a} {
                return 1
            }
        }
    }
    return 0
}

proc supportsTLS {ip} {
    set hasABBAOutside 0
    set parts [split $ip {[]}]
    for {set i 0} {$i < [llength $parts]} {incr i} {
        set part [lindex $parts $i]
        if {[expr {$i % 2}] == 0} {
            if {[hasABBA $part]} {
                set hasABBAOutside 1
            }
        } else {
            if {[hasABBA $part]} {
                return 0
            }
        }
    }
    return $hasABBAOutside
}

set file [open "input.txt" r]
set count 0
while {[gets $file line] >= 0} {
    if {[supportsTLS $line]} {
        incr count
    }
}
close $file
puts $count