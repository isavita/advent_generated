
set f [open "input.txt" r]
set scores {}
array set m {( ) \[ \] \{ \} < >}
array set p {( 1 \[ 2 \{ 3 < 4}
while {[gets $f line] >= 0} {
    set st {}
    set bad 0
    foreach c [split $line ""] {
        if {[info exists m($c)]} {
            lappend st $c
        } else {
            if {[llength $st] && $c eq $m([lindex $st end])} {
                set st [lreplace $st end end]
            } else {
                set bad 1
                break
            }
        }
    }
    if {!$bad && [llength $st]} {
        set s 0
        foreach c [lreverse $st] {
            set s [expr {$s * 5 + $p($c)}]
        }
        lappend scores $s
    }
}
close $f
set scores [lsort -integer $scores]
puts [lindex $scores [expr {[llength $scores] / 2}]]
