
#!/usr/bin/tclsh

# Part 1 and 2 of Advent of Code 2019 Day 17
# Set and Forget: ASCII Intcode Challenge

# Intcode computer engine
proc get_val {mode param rb mem_ref} {
    upvar 1 $mem_ref mem
    if {$mode == 0} {
        return [expr {[dict exists $mem $param] ? [dict get $mem $param] : 0}]
    } elseif {$mode == 1} {
        return $param
    } elseif {$mode == 2} {
        set addr [expr {$rb + $param}]
        return [expr {[dict exists $mem $addr] ? [dict get $mem $addr] : 0}]
    }
}

proc set_val {mode param rb val mem_ref} {
    upvar 1 $mem_ref mem
    if {$mode == 0} {
        dict set mem $param $val
    } elseif {$mode == 2} {
        dict set mem [expr {$rb + $param}] $val
    }
}

proc run_intcode {program_dict inputs} {
    set mem $program_dict
    set ip 0; set rb 0; set input_idx 0; set outputs {}
    while {1} {
        set instr [dict get $mem $ip]
        set opcode [expr {$instr % 100}]
        set m1 [expr {($instr / 100) % 10}]
        set m2 [expr {($instr / 1000) % 10}]
        set m3 [expr {($instr / 10000) % 10}]
        if {$opcode == 99} break
        switch $opcode {
            1 {
                set v1 [get_val $m1 [dict get $mem [expr {$ip + 1}]] $rb mem]
                set v2 [get_val $m2 [dict get $mem [expr {$ip + 2}]] $rb mem]
                set_val $m3 [dict get $mem [expr {$ip + 3}]] $rb [expr {$v1 + $v2}] mem
                incr ip 4
            }
            2 {
                set v1 [get_val $m1 [dict get $mem [expr {$ip + 1}]] $rb mem]
                set v2 [get_val $m2 [dict get $mem [expr {$ip + 2}]] $rb mem]
                set_val $m3 [dict get $mem [expr {$ip + 3}]] $rb [expr {$v1 * $v2}] mem
                incr ip 4
            }
            3 {
                set_val $m1 [dict get $mem [expr {$ip + 1}]] $rb [lindex $inputs $input_idx] mem
                incr input_idx; incr ip 2
            }
            4 {
                lappend outputs [get_val $m1 [dict get $mem [expr {$ip + 1}]] $rb mem]
                incr ip 2
            }
            5 {
                if {[get_val $m1 [dict get $mem [expr {$ip + 1}]] $rb mem] != 0} {
                    set ip [get_val $m2 [dict get $mem [expr {$ip + 2}]] $rb mem]
                } else { incr ip 3 }
            }
            6 {
                if {[get_val $m1 [dict get $mem [expr {$ip + 1}]] $rb mem] == 0} {
                    set ip [get_val $m2 [dict get $mem [expr {$ip + 2}]] $rb mem]
                } else { incr ip 3 }
            }
            7 {
                set v1 [get_val $m1 [dict get $mem [expr {$ip + 1}]] $rb mem]
                set v2 [get_val $m2 [dict get $mem [expr {$ip + 2}]] $rb mem]
                set_val $m3 [dict get $mem [expr {$ip + 3}]] $rb [expr {$v1 < $v2 ? 1 : 0}] mem
                incr ip 4
            }
            8 {
                set v1 [get_val $m1 [dict get $mem [expr {$ip + 1}]] $rb mem]
                set v2 [get_val $m2 [dict get $mem [expr {$ip + 2}]] $rb mem]
                set_val $m3 [dict get $mem [expr {$ip + 3}]] $rb [expr {$v1 == $v2 ? 1 : 0}] mem
                incr ip 4
            }
            9 {
                set rb [expr {$rb + [get_val $m1 [dict get $mem [expr {$ip + 1}]] $rb mem]}]
                incr ip 2
            }
        }
    }
    return $outputs
}

# Path compression logic (Main, A, B, C)
proc token_length {tokens} {
    if {[llength $tokens] == 0} { return 0 }
    return [string length [join $tokens ","]]
}

proc append_main {main val} {
    return [expr {$main eq "" ? $val : "$main,$val"}]
}

proc find_solution {tokens main funcs} {
    if {[string length $main] > 20} { return "" }
    if {[llength $tokens] == 0} { return [list $main $funcs] }
    # Try using already defined functions
    for {set i 0} {$i < 3} {incr i} {
        set f [lindex $funcs $i]
        if {$f ne ""} {
            set len [llength $f]
            if {[lrange $tokens 0 [expr {$len-1}]] == $f} {
                set res [find_solution [lrange $tokens $len end] [append_main $main [lindex {A B C} $i]] $funcs]
                if {$res ne ""} { return $res }
            }
        }
    }
    # Try defining a new function
    for {set i 0} {$i < 3} {incr i} {
        if {[lindex $funcs $i] eq ""} {
            for {set len 1} {$len <= [llength $tokens]} {incr len} {
                set nf [lrange $tokens 0 [expr {$len-1}]]
                if {[token_length $nf] > 20} break
                set next_f $funcs; lset next_f $i $nf
                set res [find_solution [lrange $tokens $len end] [append_main $main [lindex {A B C} $i]] $next_f]
                if {$res ne ""} { return $res }
            }
            break
        }
    }
    return ""
}

proc main {} {
    if {![file exists "input.txt"]} return
    set f [open "input.txt" r]; set data [read $f]; close $f
    set idx 0; set program_dict [dict create]
    foreach val [split [string trim $data] ","] {
        dict set program_dict $idx [expr {wide($val)}]; incr idx
    }

    # Part 1: Grid extraction and alignment parameters
    set out1 [run_intcode $program_dict {}]
    set x 0; set y 0; set max_x 0; set grid [dict create]
    foreach code $out1 {
        if {$code == 10} { if {$x > $max_x} {set max_x $x}; set x 0; incr y } else {
            set c [format %c $code]
            if {[string first $c "#^v<>"] != -1} {
                dict set grid $x,$y "#"
                if {$c ne "#"} {
                    set sx $x; set sy $y; set sdx 0; set sdy 0
                    if {$c eq "^"} {set sdy -1} elseif {$c eq "v"} {set sdy 1} \
                    elseif {$c eq "<"} {set sdx -1} elseif {$c eq ">"} {set sdx 1}
                }
            } else { dict set grid $x,$y "." }
            incr x
        }
    }
    set max_y $y; set sum 0
    for {set i 1} {$i < $max_x - 1} {incr i} {
        for {set j 1} {$j < $max_y - 1} {incr j} {
            if {[dict get $grid $i,$j] eq "#"} {
                set is 1
                foreach {dx dy} {0 1 0 -1 1 0 -1 0} {
                    if {![dict exists $grid [expr {$i+$dx}],[expr {$j+$dy}]] || [dict get $grid [expr {$i+$dx}],[expr {$j+$dy}]] ne "#"} {
                        set is 0; break
                    }
                }
                if {$is} { incr sum [expr {$i * $j}] }
            }
        }
    }
    puts "Part 1: $sum"

    # Part 2: Path generation
    set path {}; set cx $sx; set cy $sy; set cdx $sdx; set cdy $sdy
    while {1} {
        set d 0
        while {[dict exists $grid [expr {$cx+$cdx}],[expr {$cy+$cdy}]] && [dict get $grid [expr {$cx+$cdx}],[expr {$cy+$cdy}]] eq "#"} {
            incr d; set cx [expr {$cx+$cdx}]; set cy [expr {$cy+$cdy}]
        }
        if {$d > 0} { lappend path $d }
        set lx $cdy; set ly [expr {-$cdx}]; set rx [expr {-$cdy}]; set ry $cdx
        if {[dict exists $grid [expr {$cx+$lx}],[expr {$cy+$ly}]] && [dict get $grid [expr {$cx+$lx}],[expr {$cy+$ly}]] eq "#"} {
            lappend path "L"; set cdx $lx; set cdy $ly
        } elseif {[dict exists $grid [expr {$cx+$rx}],[expr {$cy+$ry}]] && [dict get $grid [expr {$cx+$rx}],[expr {$cy+$ry}]] eq "#"} {
            lappend path "R"; set cdx $rx; set cdy $ry
        } else { break }
    }

    # Path compression
    set sol [find_solution $path "" [list "" "" ""]]
    if {$sol eq ""} return
    set s_in "[lindex $sol 0]\n[join [lindex [lindex $sol 1] 0] ","]\n[join [lindex [lindex $sol 1] 1] ","]\n[join [lindex [lindex $sol 1] 2] ","]\nn\n"
    set a_in {}; foreach c [split $s_in ""] { scan $c %c v; lappend a_in $v }

    # Run for Part 2
    dict set program_dict 0 2
    set out2 [run_intcode $program_dict $a_in]
    puts "Part 2: [lindex $out2 end]"
}

main
