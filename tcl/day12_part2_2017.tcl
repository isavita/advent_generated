
set f [open input.txt r]
while {[gets $f line] >= 0} {
    set nums [regexp -all -inline {\d+} $line]
    set u [lindex $nums 0]
    foreach v [lrange $nums 1 end] {
        dict lappend adj $u $v
        dict lappend adj $v $u
    }
}
close $f

set groups 0
foreach node [dict keys $adj] {
    if {![info exists visited($node)]} {
        incr groups
        set todo [list $node]
        while {[llength $todo]} {
            set u [lindex $todo end]
            set todo [lrange $todo 0 end-1]
            foreach v [dict get $adj $u] {
                if {![info exists visited($v)]} {
                    set visited($v) 1
                    lappend todo $v
                }
            }
        }
    }
}
puts $groups
