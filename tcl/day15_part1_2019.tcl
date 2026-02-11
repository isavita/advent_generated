
proc get_m {m a} { expr {[dict exists $m $a] ? [dict get $m $a] : 0} }

proc step {m ip rb in} {
    while 1 {
        set s [get_m $m $ip]
        set op [expr {$s % 100}]
        set m1 [expr {$s / 100 % 10}]
        set m2 [expr {$s / 1000 % 10}]
        set m3 [expr {$s / 10000 % 10}]
        set p1 [get_m $m [expr {$ip + 1}]]
        set p2 [get_m $m [expr {$ip + 2}]]
        set p3 [get_m $m [expr {$ip + 3}]]
        set v1 [expr {$m1 == 0 ? [get_m $m $p1] : ($m1 == 1 ? $p1 : [get_m $m [expr {$rb + $p1}]])}]
        set v2 [expr {$m2 == 0 ? [get_m $m $p2] : ($m2 == 1 ? $p2 : [get_m $m [expr {$rb + $p2}]])}]
        if {$op == 1} {
            dict set m [expr {$m3 == 0 ? $p3 : $rb + $p3}] [expr {$v1 + $v2}]
            set ip [expr {$ip + 4}]
        } elseif {$op == 2} {
            dict set m [expr {$m3 == 0 ? $p3 : $rb + $p3}] [expr {$v1 * $v2}]
            set ip [expr {$ip + 4}]
        } elseif {$op == 3} {
            dict set m [expr {$m1 == 0 ? $p1 : $rb + $p1}] $in
            set ip [expr {$ip + 2}]
        } elseif {$op == 4} {
            return [list $m [expr {$ip + 2}] $rb $v1]
        } elseif {$op == 5} {
            set ip [expr {$v1 != 0 ? $v2 : $ip + 3}]
        } elseif {$op == 6} {
            set ip [expr {$v1 == 0 ? $v2 : $ip + 3}]
        } elseif {$op == 7} {
            dict set m [expr {$m3 == 0 ? $p3 : $rb + $p3}] [expr {$v1 < $v2 ? 1 : 0}]
            set ip [expr {$ip + 4}]
        } elseif {$op == 8} {
            dict set m [expr {$m3 == 0 ? $p3 : $rb + $p3}] [expr {$v1 == $v2 ? 1 : 0}]
            set ip [expr {$ip + 4}]
        } elseif {$op == 9} {
            set rb [expr {$rb + $v1}]
            set ip [expr {$ip + 2}]
        } else {
            return [list $m $ip $rb -1]
        }
    }
}

set f [open "input.txt" r]
set d [split [string trim [read $f]] ,]
close $f

set m [dict create]
set i 0
foreach val $d {dict set m $i $val; incr i}

set q [list [list 0 0 0 $m 0 0]]
set v [dict create "0,0" 1]
set h 0

while {$h < [llength $q]} {
    lassign [lindex $q $h] cx cy cd cm ci cr
    incr h
    foreach {dr dx dy} {1 0 -1 2 0 1 3 -1 0 4 1 0} {
        set nx [expr {$cx + $dx}]
        set ny [expr {$cy + $dy}]
        if {[dict exists $v "$nx,$ny"]} continue
        lassign [step $cm $ci $cr $dr] nm ni nr o
        dict set v "$nx,$ny" 1
        if {$o == 1} {
            lappend q [list $nx $ny [expr {$cd + 1}] $nm $ni $nr]
        } elseif {$o == 2} {
            puts [expr {$cd + 1}]
            exit
        }
    }
}
