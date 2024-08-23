set file [open "input.txt" r]
set operations [split [read $file] "\n"]
close $file

proc swapPosition {s x y} {
    set chars [split $s ""]
    set temp [lindex $chars $x]
    set chars [lreplace $chars $x $x [lindex $chars $y]]
    set chars [lreplace $chars $y $y $temp]
    return [join $chars ""]
}

proc swapLetter {s x y} {
    set chars [split $s ""]
    for {set i 0} {$i < [llength $chars]} {incr i} {
        if {[lindex $chars $i] eq $x} {
            set chars [lreplace $chars $i $i $y]
        } elseif {[lindex $chars $i] eq $y} {
            set chars [lreplace $chars $i $i $x]
        }
    }
    return [join $chars ""]
}

proc rotate {s steps} {
    set len [string length $s]
    set steps [expr {$steps % $len}]
    return [string range $s [expr {$len - $steps}] end][string range $s 0 [expr {$len - $steps - 1}]]
}

proc reversePositions {s x y} {
    set chars [split $s ""]
    set reversed [lrange $chars $x $y]
    set reversed [join [lreverse $reversed] ""]
    set chars [lreplace $chars $x $y $reversed]
    return [join $chars ""]
}

proc movePosition {s x y} {
    set chars [split $s ""]
    set char [lindex $chars $x]
    set chars [lreplace $chars $x $x]
    set chars [linsert $chars $y $char]
    return [join $chars ""]
}

proc rotateBasedOnPosition {s x} {
    set index [string first $x $s]
    set steps [expr {$index + 1}]
    if {$index >= 4} {incr steps}
    return [rotate $s $steps]
}

set password "abcdefgh"
foreach op $operations {
    if {[regexp {swap position (\d+) with position (\d+)} $op -> x y]} {
        set password [swapPosition $password $x $y]
    } elseif {[regexp {swap letter (\w) with letter (\w)} $op -> x y]} {
        set password [swapLetter $password $x $y]
    } elseif {[regexp {rotate left (\d+) steps?} $op -> steps]} {
        set password [rotate $password -$steps]
    } elseif {[regexp {rotate right (\d+) steps?} $op -> steps]} {
        set password [rotate $password $steps]
    } elseif {[regexp {rotate based on position of letter (\w)} $op -> x]} {
        set password [rotateBasedOnPosition $password $x]
    } elseif {[regexp {reverse positions (\d+) through (\d+)} $op -> x y]} {
        set password [reversePositions $password $x $y]
    } elseif {[regexp {move position (\d+) to position (\d+)} $op -> x y]} {
        set password [movePosition $password $x $y]
    }
}
puts $password