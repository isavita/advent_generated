
proc read_all {path} {
    set fd [open $path r]
    set content [read $fd]
    close $fd
    return [string trim $content]
}

proc num {w} {
    set n 0
    foreach digit $w {
        set n [expr {$n * 10 + $digit}]
    }
    return $n
}

proc main {} {
    set k {}
    set l {}
    set m {}
    set lines [split [read_all "input.txt"] "\n"]
    
    for {set i 0} {$i < [llength $lines]} {incr i} {
        set line [lindex $lines $i]
        
        if {[expr {$i % 18}] == 4} {
            lappend l [lindex [split $line] 2]
        } elseif {[expr {$i % 18}] == 5} {
            lappend k [lindex [split $line] 2]
        } elseif {[expr {$i % 18}] == 15} {
            lappend m [lindex [split $line] 2]
        }
    }

    set constraints {}
    set stack {}
    for {set i 0} {$i < [llength $l]} {incr i} {
        if {[lindex $l $i] == 1} {
            lappend stack $i
        } elseif {[lindex $l $i] == 26} {
            set pop [lindex $stack end]
            set stack [lrange $stack 0 end-1]
            set constraints [concat $constraints [list $pop [list $i [expr {[lindex $m $pop] + [lindex $k $i]}]]]]
        }
    }

    set min_val [lrepeat 14 0]
    foreach {pop constraint} $constraints {
        set vmin 1
        while {[expr {$vmin + [lindex $constraint 1] < 1}]} {
            incr vmin
        }
        lset min_val $pop $vmin
        lset min_val [lindex $constraint 0] [expr {$vmin + [lindex $constraint 1]}]
    }

    puts [num $min_val]
}

main
