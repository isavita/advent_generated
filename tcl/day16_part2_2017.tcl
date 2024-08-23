set file [open "input.txt" r]
set moves [split [read $file] ","]
close $file

set programs [split "abcdefghijklmnop" ""]
set seen [dict create]

proc applyMove {programs move} {
    if {[string index $move 0] eq "s"} {
        set x [string range $move 1 end]
        set x [expr {$x + 0}]
        set programs [concat [lrange $programs end-[expr {$x-1}] end] [lrange $programs 0 end-$x]]
    } elseif {[string index $move 0] eq "x"} {
        set parts [split [string range $move 1 end] "/"]
        set a [lindex $parts 0]
        set b [lindex $parts 1]
        set temp [lindex $programs $a]
        lset programs $a [lindex $programs $b]
        lset programs $b $temp
    } elseif {[string index $move 0] eq "p"} {
        set parts [split [string range $move 1 end] "/"]
        set a [lsearch $programs [lindex $parts 0]]
        set b [lsearch $programs [lindex $parts 1]]
        set temp [lindex $programs $a]
        lset programs $a [lindex $programs $b]
        lset programs $b $temp
    }
    return $programs
}

for {set i 0} {$i < 1000000000} {incr i} {
    set key [join $programs ""]
    if {[dict exists $seen $key]} {
        set cycleLength [expr {$i - [dict get $seen $key]}]
        set remaining [expr {(1000000000 - $i) % $cycleLength}]
        for {set j 0} {$j < $remaining} {incr j} {
            foreach move $moves {
                set programs [applyMove $programs $move]
            }
        }
        break
    }
    dict set seen $key $i
    foreach move $moves {
        set programs [applyMove $programs $move]
    }
}

puts [join $programs ""]