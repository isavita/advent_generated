
array set grid {}
array set dp {}
set nr 0
set nc 0
set dirs {{1 0} {-1 0} {0 1} {0 -1}}

proc dfs {r c} {
    global grid dp nr nc dirs

    if {[info exists dp($r,$c)] && $dp($r,$c) != -1} {
        return $dp($r,$c)
    }

    set h $grid($r,$c)

    if {$h == 9} {
        set dp($r,$c) 1
        return 1
    }

    set sum 0
    foreach dir $dirs {
        set dr [lindex $dir 0]
        set dc [lindex $dir 1]
        set nr2 [expr {$r + $dr}]
        set nc2 [expr {$c + $dc}]

        if {$nr2 >= 0 && $nr2 < $nr && $nc2 >= 0 && $nc2 < $nc} {
            if {$grid($nr2,$nc2) == [expr {$h + 1}]} {
                set sum [expr {$sum + [dfs $nr2 $nc2]}]
            }
        }
    }

    set dp($r,$c) $sum
    return $sum
}

proc main {} {
    global grid dp nr nc dirs

    set filename "input.txt"
    set f [open $filename r]
    set lines [split [read $f] "\n"]
    close $f

    if {[llength $lines] > 0 && [string trim [lindex $lines end]] eq ""} {
        set lines [lrange $lines 0 [expr {[llength $lines] - 2}]]
    }

    set nr [llength $lines]
    if {$nr == 0} {
        puts 0
        return
    }
    set nc [string length [lindex $lines 0]]
    if {$nc == 0} {
         puts 0
         return
    }

    array set grid {}
    array set dp {}

    for {set r 0} {$r < $nr} {incr r} {
        set line [lindex $lines $r]
        for {set c 0} {$c < $nc} {incr c} {
            set digit [string index $line $c]
            set grid($r,$c) $digit
            set dp($r,$c) -1
        }
    }

    set total 0
    for {set r 0} {$r < $nr} {incr r} {
        for {set c 0} {$c < $nc} {incr c} {
            if {$grid($r,$c) == 0} {
                set total [expr {$total + [dfs $r $c]}]
            }
        }
    }
    puts $total
}

main
