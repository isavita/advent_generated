
proc calculate_ore {chem amount reactions ingredients surplus_name} {
    upvar $surplus_name surplus
    if {$chem eq "ORE"} {return $amount}
    set s 0
    if {[dict exists $surplus $chem]} {set s [dict get $surplus $chem]}
    if {$s >= $amount} {
        dict set surplus $chem [expr {$s - $amount}]
        return 0
    }
    set amount [expr {$amount - $s}]
    set p_amt [dict get $reactions $chem]
    set times [expr {($amount + $p_amt - 1) / $p_amt}]
    set ore 0
    foreach ing [dict get $ingredients $chem] {
        lassign $ing i_amt i_name
        set ore [expr {$ore + [calculate_ore $i_name [expr {$i_amt * $times}] $reactions $ingredients surplus]}]
    }
    dict set surplus $chem [expr {$times * $p_amt - $amount}]
    return $ore
}

set f [open "input.txt" r]
set reactions [dict create]
set ingredients [dict create]
while {[gets $f line] >= 0} {
    if {![regexp {(.+) => (.+)} $line -> left right]} continue
    lassign [regexp -all -inline {\d+ \w+} $right] r_val
    lassign [split $r_val] r_amt r_name
    dict set reactions $r_name $r_amt
    set ings {}
    foreach item [regexp -all -inline {\d+ \w+} $left] {
        lassign [split $item] i_amt i_name
        lappend ings [list $i_amt $i_name]
    }
    dict set ingredients $r_name $ings
}
close $f

set limit 1000000000000
set low 0
set high $limit
set ans 0
while {$low <= $high} {
    set mid [expr {($low + $high) / 2}]
    if {$mid == 0} {
        set low 1
        continue
    }
    set s [dict create]
    if {[calculate_ore FUEL $mid $reactions $ingredients s] <= $limit} {
        set ans $mid
        set low [expr {$mid + 1}]
    } else {
        set high [expr {$mid - 1}]
    }
}
puts $ans
