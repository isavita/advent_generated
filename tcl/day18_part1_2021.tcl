
proc main {} {
    set lines [list]
    set fileId [open "input.txt" r]
    while {[gets $fileId line] != -1} {
        lappend lines $line
    }
    close $fileId

    if {[llength lines] == 0} {
        return
    }

    set sum [lindex $lines 0]
    for {set i 1} {$i < [llength $lines]} {incr i} {
        set nextNum [lindex $lines $i]
        set sum [add $sum $nextNum]
    }

    puts [magnitude $sum]
}

proc add {s1 s2} {
    set result "\[${s1},${s2}\]"
    return [reduce $result]
}

proc reduce {s} {
    while {true} {
        set exploded [explode $s]
        if {$exploded ne $s} {
            set s $exploded
            continue
        }
        set split [split $s]
        if {$split ne $s} {
            set s $split
            continue
        }
        break
    }
    return $s
}

proc explode {s} {
    set depth 0
    set len [string length $s]
    for {set i 0} {$i < $len} {incr i} {
        set c [string index $s $i]
        if {$c eq "\["} {
            incr depth
        } elseif {$c eq "\]"} {
            incr depth -1
        } elseif {$depth > 4 && [string is digit -strict $c]} {
            set j $i
            while {$j < $len && [string is digit -strict [string index $s $j]]} {
                incr j
            }
            if {$j < $len && [string index $s $j] eq ","} {
                set k [expr {$j + 1}]
                if {$k < $len && [string is digit -strict [string index $s $k]]} {
                     set l $k
                     while {$l < $len && [string is digit -strict [string index $s $l]]} {
                         incr l
                     }
                     if {$l < $len && [string index $s $l] eq "\]"} {
                         set leftVal [string range $s $i [expr {$j - 1}]]
                         set rightVal [string range $s $k [expr {$l - 1}]]

                         set prefix [string range $s 0 [expr {$i - 2}]]
                         set suffix [string range $s [expr {$l + 1}] end]

                         set newPrefix [addToRightmost $prefix $leftVal]
                         set newSuffix [addToLeftmost $suffix $rightVal]

                         return "${newPrefix}0${newSuffix}"
                     }
                 }
             }
         }
    }
    return $s
}

proc addToRightmost {s num} {
    set len [string length $s]
    for {set i [expr {$len - 1}]} {$i >= 0} {incr i -1} {
        if {[string is digit -strict [string index $s $i]]} {
            set j $i
            while {$j >= 0 && [string is digit -strict [string index $s $j]]} {
                incr j -1
            }
            set n [string range $s [expr {$j + 1}] $i]
            set newVal [expr {$n + $num}]
            return [string replace $s [expr {$j + 1}] $i $newVal]
        }
    }
    return $s
}

proc addToLeftmost {s num} {
    set len [string length $s]
    for {set i 0} {$i < $len} {incr i} {
        if {[string is digit -strict [string index $s $i]]} {
            set j $i
            while {$j < $len && [string is digit -strict [string index $s $j]]} {
                incr j
            }
            set n [string range $s $i [expr {$j - 1}]]
            set newVal [expr {$n + $num}]
            return [string replace $s $i [expr {$j - 1}] $newVal]
        }
    }
    return $s
}

proc split {s} {
    set idx -1
    if {[regexp -indices -- {\d{2,}} $s matchIdx]} {
        set idx [lindex $matchIdx 0]
    } else {
        return $s
    }

    set i $idx
    set j $i
    set len [string length $s]
    while {$j < $len && [string is digit -strict [string index $s $j]]} {
        incr j
    }

    set num [string range $s $i [expr {$j - 1}]]
    set left [expr {int($num / 2)}]
    set right [expr {$num - $left}]
    set replacement "\[${left},${right}\]"

    return [string replace $s $i [expr {$j - 1}] $replacement]
}

proc magnitude {s} {
    if {[string index $s 0] ne "\["} {
        return $s
    }

    set depth 0
    set commaIndex -1
    set len [string length $s]
    for {set i 0} {$i < $len} {incr i} {
        set c [string index $s $i]
        if {$c eq "\["} {
            incr depth
        } elseif {$c eq "\]"} {
            incr depth -1
        } elseif {$c eq "," && $depth == 1} {
            set commaIndex $i
            break
        }
    }

    set leftPart [string range $s 1 [expr {$commaIndex - 1}]]
    set rightPart [string range $s [expr {$commaIndex + 1}] [expr {$len - 2}]]

    return [expr {3 * [magnitude $leftPart] + 2 * [magnitude $rightPart]}]
}

main
