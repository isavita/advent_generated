
set f [open input.txt r]
set ranges {}
set mode ranges
while {[gets $f line] != -1} {
    set line [string trim $line]
    if {$line eq ""} {
        if {$mode eq "ranges"} {
            set mode tickets
            set ranges [lsort -index 0 -integer $ranges]
            set merged {}
            foreach range $ranges {
                lassign $range lo hi
                if {[llength $merged] == 0 || $lo > [lindex $merged end 1]} {
                    lappend merged [list $lo $hi]
                } elseif {$hi > [lindex $merged end 1]} {
                    lset merged end 1 $hi
                }
            }
            set ranges $merged
        }
        continue
    }
    if {$mode eq "ranges"} {
        lassign [split $line -] lo hi
        lappend ranges [list $lo $hi]
    } else {
        set fresh 0
        foreach range $ranges {
            lassign $range lo hi
            if {$line >= $lo && $line <= $hi} {
                incr fresh
                break
            }
        }
        incr total $fresh
    }
}
close $f
puts "Number of fresh ingredients: $total"
