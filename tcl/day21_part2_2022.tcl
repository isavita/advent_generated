
# Tcl solution
array set val {}
array set has_val {}
array set left {}
array set right {}
array set op {}

set f [open "input.txt"]
while {[gets $f line] >= 0} {
    if {[regexp {^(\w{1,4}): ([+-]?\d+)$} $line -> name num]} {
        set val($name) $num
        set has_val($name) 1
    } elseif {[regexp {^(\w{1,4}): (\w{1,4}) ([+\-*/]) (\w{1,4})$} $line -> name l o r]} {
        set left($name) $l
        set right($name) $r
        set op($name) $o
        set has_val($name) 0
    }
}
close $f

proc solve {n} {
    upvar 1 val val has_val has_val left left right right op op
    if {[info exists has_val($n)] && $has_val($n)} {
        return [list 1 $val($n)]
    }
    if {![info exists left($n)]} {return [list 0 0]}
    set lres [solve $left($n)]
    set rres [solve $right($n)]
    if {[lindex $lres 0] && [lindex $rres 0]} {
        set l [lindex $lres 1]
        set r [lindex $rres 1]
        switch -- $op($n) {
            + {set v [expr {$l + $r}]}
            - {set v [expr {$l - $r}]}
            * {set v [expr {$l * $r}]}
            / {set v [expr {$l / $r}]}
            = {set v [expr {$l == $r}]}
        }
        return [list 1 $v]
    }
    return [list 0 0]
}

proc expect {n target} {
    upvar 1 val val has_val has_val left left right right op op
    if {$n eq "humn"} {return $target}
    set lres [solve $left($n)]
    set rres [solve $right($n)]
    if {![lindex $lres 0]} {
        set r [lindex $rres 1]
        switch -- $op($n) {
            + {return [expect $left($n) [expr {$target - $r}]]}
            - {return [expect $left($n) [expr {$target + $r}]]}
            * {return [expect $left($n) [expr {$target / $r}]]}
            / {return [expect $left($n) [expr {$target * $r}]]}
            = {return [expect $left($n) $r]}
        }
    }
    if {![lindex $rres 0]} {
        set l [lindex $lres 1]
        switch -- $op($n) {
            + {return [expect $right($n) [expr {$target - $l}]]}
            - {return [expect $right($n) [expr {$l - $target}]]}
            * {return [expect $right($n) [expr {$target / $l}]]}
            / {return [expect $right($n) [expr {$l / $target}]]}
            = {return [expect $right($n) $l]}
        }
    }
    puts stderr "Error: cannot resolve $n"
    exit 1
}

set has_val(humn) 0
set op(root) =

puts [expect root 0]
