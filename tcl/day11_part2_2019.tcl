
proc get_m {m_var addr} {
    upvar $m_var m
    if {[dict exists $m $addr]} { return [dict get $m $addr] }
    return 0
}

proc get_addr {m_var ip rb mode offset} {
    upvar $m_var m
    set param [get_m m [expr {$ip + $offset}]]
    if {$mode == 0} { return $param }
    if {$mode == 1} { return [expr {$ip + $offset}] }
    if {$mode == 2} { return [expr {$rb + $param}] }
}

proc run_robot {program start_color} {
    set mem $program
    set ip 0
    set rb 0
    set x 0
    set y 0
    set dir 0
    set panels [dict create "0,0" $start_color]
    set painted [dict create]
    set out_state 0

    while {1} {
        set instr [get_m mem $ip]
        set op [expr {$instr % 100}]
        set m [list [expr {$instr/100%10}] [expr {$instr/1000%10}] [expr {$instr/10000%10}]]

        if {$op == 99} break
        switch $op {
            1 {
                set v1 [get_m mem [get_addr mem $ip $rb [lindex $m 0] 1]]
                set v2 [get_m mem [get_addr mem $ip $rb [lindex $m 1] 2]]
                dict set mem [get_addr mem $ip $rb [lindex $m 2] 3] [expr {$v1 + $v2}]
                incr ip 4
            }
            2 {
                set v1 [get_m mem [get_addr mem $ip $rb [lindex $m 0] 1]]
                set v2 [get_m mem [get_addr mem $ip $rb [lindex $m 1] 2]]
                dict set mem [get_addr mem $ip $rb [lindex $m 2] 3] [expr {$v1 * $v2}]
                incr ip 4
            }
            3 {
                set cur [expr {[dict exists $panels "$x,$y"] ? [dict get $panels "$x,$y"] : 0}]
                dict set mem [get_addr mem $ip $rb [lindex $m 0] 1] $cur
                incr ip 2
            }
            4 {
                set out [get_m mem [get_addr mem $ip $rb [lindex $m 0] 1]]
                if {$out_state == 0} {
                    dict set panels "$x,$y" $out
                    dict set painted "$x,$y" 1
                    set out_state 1
                } else {
                    if {$out == 0} { set dir [expr {($dir + 3) % 4}] } else { set dir [expr {($dir + 1) % 4}] }
                    if {$dir == 0} { incr y -1 } elseif {$dir == 1} { incr x } elseif {$dir == 2} { incr y } else { incr x -1 }
                    set out_state 0
                }
                incr ip 2
            }
            5 {
                set v1 [get_m mem [get_addr mem $ip $rb [lindex $m 0] 1]]
                if {$v1 != 0} { set ip [get_m mem [get_addr mem $ip $rb [lindex $m 1] 2]] } else { incr ip 3 }
            }
            6 {
                set v1 [get_m mem [get_addr mem $ip $rb [lindex $m 0] 1]]
                if {$v1 == 0} { set ip [get_m mem [get_addr mem $ip $rb [lindex $m 1] 2]] } else { incr ip 3 }
            }
            7 {
                set v1 [get_m mem [get_addr mem $ip $rb [lindex $m 0] 1]]
                set v2 [get_m mem [get_addr mem $ip $rb [lindex $m 1] 2]]
                dict set mem [get_addr mem $ip $rb [lindex $m 2] 3] [expr {$v1 < $v2 ? 1 : 0}]
                incr ip 4
            }
            8 {
                set v1 [get_m mem [get_addr mem $ip $rb [lindex $m 0] 1]]
                set v2 [get_m mem [get_addr mem $ip $rb [lindex $m 1] 2]]
                dict set mem [get_addr mem $ip $rb [lindex $m 2] 3] [expr {$v1 == $v2 ? 1 : 0}]
                incr ip 4
            }
            9 {
                set v1 [get_m mem [get_addr mem $ip $rb [lindex $m 0] 1]]
                set rb [expr {$rb + $v1}]
                incr ip 2
            }
        }
    }
    return [list $panels $painted]
}

set f [open "input.txt" r]
set data [read $f]
close $f
set program [dict create]
set i 0
foreach val [split [string trim $data] ","] {
    dict set program $i $val
    incr i
}

lassign [run_robot $program 0] p1_panels p1_painted
puts "Part One: [dict size $p1_painted]"

lassign [run_robot $program 1] p2_panels p2_painted
set minx 1e9; set maxx -1e9; set miny 1e9; set maxy -1e9
dict for {coord color} $p2_panels {
    lassign [split $coord ","] cx cy
    if {$cx < $minx} {set minx $cx}; if {$cx > $maxx} {set maxx $cx}
    if {$cy < $miny} {set miny $cy}; if {$cy > $maxy} {set maxy $cy}
}

puts "Part Two:"
for {set j $miny} {$j <= $maxy} {incr j} {
    for {set i $minx} {$i <= $maxx} {incr i} {
        set c [expr {[dict exists $p2_panels "$i,$j"] ? [dict get $p2_panels "$i,$j"] : 0}]
        puts -nonewline [expr {$c == 1 ? "#" : " "}]
    }
    puts ""
}
