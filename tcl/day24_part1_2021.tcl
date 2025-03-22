
proc read_file {filename} {
    set fp [open $filename r]
    set content [read $fp]
    close $fp
    return $content
}

proc num {list} {
    set n 0
    foreach i $list {
        set n [expr {$n * 10 + $i}]
    }
    return $n
}

proc main {} {
    set k {}
    set l {}
    set m {}

    set lines [split [read_file "input.txt"] "\n"]
    for {set i 0} {$i < [llength $lines]} {incr i} {
        set line [lindex $lines $i]
        if {[expr {$i % 18}] == 4} {
            lappend l [lindex [split $line] end]
        } elseif {[expr {$i % 18}] == 5} {
            lappend k [lindex [split $line] end]
        } elseif {[expr {$i % 18}] == 15} {
            lappend m [lindex [split $line] end]
        }
    }

    set constraints {}
    set stack {}
    for {set i 0} {$i < [llength $l]} {incr i} {
        if {[lindex $l $i] == 1} {
            lappend stack $i
        } elseif {[lindex $l $i] == 26} {
            set pop [lindex $stack end]
            set stack [lrange $stack 0 [expr {[llength $stack] - 2}]]
            set constraints [concat $constraints [list $pop [list $i [expr {[lindex $m $pop] + [lindex $k $i]}]]]]
        }
    }

    set max_vals [lrepeat 14 0]
    foreach {i constraint} $constraints {
        set vmax 9
        while {[expr {$vmax + [lindex $constraint 1]}] > 9} {
            incr vmax -1
        }
        lset max_vals $i $vmax
        lset max_vals [lindex $constraint 0] [expr {$vmax + [lindex $constraint 1]}]
    }

    puts [num $max_vals]
}

main
