
proc main {} {
    set f [open input.txt r]
    set input [read $f]
    close $f
    set state [newInitialState $input]
    set queue [list $state]
    set seen [dict create]
    while {[llength $queue]} {
        set current [lindex $queue 0]
        set queue [lrange $queue 1 end]
        if {[isDone $current]} {
            puts [dict get $current steps]
            return
        }
        set h [hashKey $current]
        if {[dict exists $seen $h]} continue
        dict set seen $h 1
        foreach next [getNextStates $current] {
            lappend queue $next
        }
    }
    puts -1
}

proc newInitialState {input} {
    set state [dict create elevatorLevel 0 steps 0]
    set floors [list [list] [list] [list] [list]]
    set idx 0
    foreach line [split [string trim $input] \n] {
        set words [regexp -all -inline {\S+} [string map {, "" . ""} $line]]
        for {set i 0} {$i < [llength $words]} {incr i} {
            set w [lindex $words $i]
            if {$w eq "generator"} {
                set m [lindex $words [expr {$i-1}]]
                lappend fl [list gen $m]
            } elseif {$w eq "microchip"} {
                set m [lindex $words [expr {$i-1}]]
                set m [lindex [split $m -] 0]
                lappend fl [list chip $m]
            }
        }
        if {[info exists fl]} {
            lset floors $idx $fl
            unset fl
        }
        incr idx
    }
    dict set state floors $floors
    return $state
}

proc hashKey {state} {
    set floors [dict get $state floors]
    set pairs [list]
    set gens [dict create]
    set chips [dict create]
    for {set fl 0} {$fl < 4} {incr fl} {
        foreach item [lindex $floors $fl] {
            lassign $item type mat
            if {$type eq "gen"} {
                dict set gens $mat $fl
            } else {
                dict set chips $mat $fl
            }
        }
    }
    dict for {mat g} $gens {
        lappend pairs [list $g [dict get $chips $mat]]
    }
    set pairs [lsort -index 0 -integer -index 1 -integer $pairs]
    return "[dict get $state elevatorLevel]$pairs"
}

proc isValid {state} {
    set floors [dict get $state floors]
    for {set fl 0} {$fl < 4} {incr fl} {
        set floor [lindex $floors $fl]
        set gens [dict create]
        foreach item $floor {
            if {[lindex $item 0] eq "gen"} {
                dict set gens [lindex $item 1] 1
            }
        }
        if {[dict size $gens] == 0} continue
        foreach item $floor {
            if {[lindex $item 0] eq "chip"} {
                if {![dict exists $gens [lindex $item 1]]} {
                    return 0
                }
            }
        }
    }
    return 1
}

proc isDone {state} {
    set floors [dict get $state floors]
    for {set fl 0} {$fl < 3} {incr fl} {
        if {[llength [lindex $floors $fl]]} {return 0}
    }
    return 1
}

proc getNextStates {state} {
    set res [list]
    set fl [dict get $state elevatorLevel]
    set floors [dict get $state floors]
    set floor [lindex $floors $fl]
    set n [llength $floor]
    set moves [list]
    for {set i 0} {$i < $n} {incr i} {
        lappend moves [list $i]
        for {set j [expr {$i+1}]} {$j < $n} {incr j} {
            lappend moves [list $i $j]
        }
    }
    set dirs [list]
    if {$fl < 3} {lappend dirs 1}
    if {$fl > 0} {lappend dirs -1}
    foreach d $dirs {
        foreach m $moves {
            set new [dict create elevatorLevel [expr {$fl+$d}] steps [expr {[dict get $state steps]+1}]]
            set nf [lrange $floors 0 3]
            set src [lindex $nf $fl]
            set dst [lindex $nf [expr {$fl+$d}]]
            set newsrc [list]
            set newdst [list]
            foreach idx $m {
                lappend newdst [lindex $src $idx]
            }
            set idxs [lsort -integer -decreasing $m]
            foreach idx $idxs {
                set src [lreplace $src $idx $idx]
            }
            lset nf $fl $src
            lset nf [expr {$fl+$d}] [concat $dst $newdst]
            dict set new floors $nf
            if {[isValid $new]} {
                lappend res $new
            }
        }
    }
    return $res
}

main
