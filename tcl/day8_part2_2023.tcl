
proc gcd {a b} {
    while {$b != 0} {
        set tmp $b
        set b [expr {$a % $b}]
        set a $tmp
    }
    return $a
}

proc lcm {a b} {
    return [expr {($a * $b) / [gcd $a $b]}]
}

proc lcm_list {nums} {
    if {[llength $nums] == 0} {
        return 0
    }
    set res [lindex $nums 0]
    foreach num [lrange $nums 1 end] {
        set res [lcm $res $num]
    }
    return $res
}

proc parse_input {input} {
    set instructions [lindex $input 0]
    set nodes [dict create]
    foreach line [lrange $input 2 end] {
        regexp {([^=]+) = \(([^,]+), ([^)]+)\)} $line _ head child1 child2
        dict set nodes $head [list $child1 $child2]
    }
    return [list $instructions $nodes]
}

proc solve {input} {
    lassign [parse_input $input] instructions nodes
    set starts {}
    dict for {node _} $nodes {
        if {[string index $node end] eq "A"} {
            lappend starts $node
        }
    }
    set steps {}
    set instructions_len [string length $instructions]
    foreach start $starts {
        set element $start
        set step 0
        while {[string index $element end] ne "Z"} {
            set instruction [string index $instructions [expr {$step % $instructions_len}]]
            if {$instruction eq "L"} {
                set element [lindex [dict get $nodes $element] 0]
            } else {
                set element [lindex [dict get $nodes $element] 1]
            }
            incr step
        }
        lappend steps $step
    }
    return [lcm_list $steps]
}

set file [open "input.txt" r]
set input [split [read $file] "\n"]
close $file
puts [solve $input]
