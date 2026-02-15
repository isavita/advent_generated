
set f [open "input.txt" r]
set data [string trim [read $f]]
close $f
set prog [split $data ,]
set mem [dict create]
set i 0
foreach p $prog { dict set mem $i [string trim $p]; incr i }

set ip 0
set rb 0
set inputs {}

proc get_mem {a} {
    global mem
    if {[dict exists $mem $a]} { return [dict get $mem $a] }
    return 0
}

proc set_mem {a v} {
    global mem
    dict set mem $a $v
}

proc get_param {m v} {
    global rb
    if {$m == 0} { return [get_mem $v] }
    if {$m == 1} { return $v }
    if {$m == 2} { return [get_mem [expr {$rb + $v}]] }
}

proc get_addr {m v} {
    global rb
    if {$m == 0} { return $v }
    if {$m == 2} { return [expr {$rb + $v}] }
}

proc run_intcode {} {
    global mem ip rb inputs
    set out ""
    while {1} {
        set instr [get_mem $ip]
        set op [expr {$instr % 100}]
        set m1 [expr {($instr / 100) % 10}]
        set m2 [expr {($instr / 1000) % 10}]
        set m3 [expr {($instr / 10000) % 10}]
        switch $op {
            1 {
                set v1 [get_param $m1 [get_mem [expr {$ip + 1}]]]
                set v2 [get_param $m2 [get_mem [expr {$ip + 2}]]]
                set a3 [get_addr $m3 [get_mem [expr {$ip + 3}]]]
                set_mem $a3 [expr {$v1 + $v2}]
                incr ip 4
            }
            2 {
                set v1 [get_param $m1 [get_mem [expr {$ip + 1}]]]
                set v2 [get_param $m2 [get_mem [expr {$ip + 2}]]]
                set a3 [get_addr $m3 [get_mem [expr {$ip + 3}]]]
                set_mem $a3 [expr {$v1 * $v2}]
                incr ip 4
            }
            3 {
                if {[llength $inputs] == 0} { return $out }
                set a1 [get_addr $m1 [get_mem [expr {$ip + 1}]]]
                set_mem $a1 [lindex $inputs 0]
                set inputs [lrange $inputs 1 end]
                incr ip 2
            }
            4 {
                set v1 [get_param $m1 [get_mem [expr {$ip + 1}]]]
                append out [format %c $v1]
                incr ip 2
            }
            5 {
                set v1 [get_param $m1 [get_mem [expr {$ip + 1}]]]
                set v2 [get_param $m2 [get_mem [expr {$ip + 2}]]]
                if {$v1 != 0} { set ip $v2 } else { incr ip 3 }
            }
            6 {
                set v1 [get_param $m1 [get_mem [expr {$ip + 1}]]]
                set v2 [get_param $m2 [get_mem [expr {$ip + 2}]]]
                if {$v1 == 0} { set ip $v2 } else { incr ip 3 }
            }
            7 {
                set v1 [get_param $m1 [get_mem [expr {$ip + 1}]]]
                set v2 [get_param $m2 [get_mem [expr {$ip + 2}]]]
                set a3 [get_addr $m3 [get_mem [expr {$ip + 3}]]]
                set_mem $a3 [expr {$v1 < $v2 ? 1 : 0}]
                incr ip 4
            }
            8 {
                set v1 [get_param $m1 [get_mem [expr {$ip + 1}]]]
                set v2 [get_param $m2 [get_mem [expr {$ip + 2}]]]
                set a3 [get_addr $m3 [get_mem [expr {$ip + 3}]]]
                set_mem $a3 [expr {$v1 == $v2 ? 1 : 0}]
                incr ip 4
            }
            9 {
                set v1 [get_param $m1 [get_mem [expr {$ip + 1}]]]
                set rb [expr {$rb + $v1}]
                incr ip 2
            }
            99 { return $out }
            default { return $out }
        }
    }
}

proc send_cmd {cmd} {
    global inputs
    foreach char [split $cmd ""] { lappend inputs [scan $char %c] }
    lappend inputs 10
}

proc parse_output {out} {
    set room ""; set doors {}; set items {}; set mode ""
    foreach line [split $out \n] {
        set line [string trim $line]
        if {[regexp {^== (.*) ==$} $line -> name]} { set room $name } \
        elseif {$line eq "Doors here lead:"} { set mode "doors" } \
        elseif {$line eq "Items here:"} { set mode "items" } \
        elseif {[string match "- *" $line]} {
            set val [string range $line 2 end]
            if {$mode eq "doors"} { lappend doors $val } { lappend items $val }
        }
    }
    return [list $room $doors $items]
}

array set opp {north south south north east west west east}
set blacklist {photons "escape pod" "molten lava" "infinite loop" "giant electromagnet"}
set doors_map [dict create]
set adj [dict create]
set all_items {}
set sensor_dir ""

set out [run_intcode]
lassign [parse_output $out] current_room ds its
dict set doors_map $current_room $ds
foreach it $its { if {$it ni $blacklist} { send_cmd "take $it"; run_intcode; lappend all_items $it } }

set visited_rooms [list $current_room]
set back_stack {}

while {1} {
    set untried ""
    foreach d [dict get $doors_map $current_room] {
        if {![dict exists $adj $current_room $d]} { set untried $d; break }
    }
    if {$untried ne ""} {
        send_cmd $untried
        set out [run_intcode]
        lassign [parse_output $out] next_room next_ds next_its
        if {[string match "*Alert!*" $out] || $next_room eq "" || $next_room eq $current_room} {
            set sensor_dir $untried
            dict set adj $current_room $untried "SENSOR"
        } else {
            dict set adj $current_room $untried $next_room
            dict set adj $next_room $opp($untried) $current_room
            if {$next_room ni $visited_rooms} {
                lappend visited_rooms $next_room
                lappend back_stack $opp($untried)
                dict set doors_map $next_room $next_ds
                foreach it $next_its { if {$it ni $blacklist} { send_cmd "take $it"; run_intcode; lappend all_items $it } }
                set current_room $next_room
            }
        }
    } elseif {[llength $back_stack] > 0} {
        set d [lindex $back_stack end]
        set back_stack [lrange $back_stack 0 end-1]
        send_cmd $d
        set out [run_intcode]
        lassign [parse_output $out] current_room ds its
    } else { break }
}

set q [list [list $current_room {}]]
set seen [list $current_room]
set path_to_cp {}
while {[llength $q] > 0} {
    set q [lassign $q item]; lassign $item curr p
    if {$curr eq "Security Checkpoint"} { set path_to_cp $p; break }
    dict for {d n} [dict get $adj $curr] {
        if {$n ne "" && $n ne "SENSOR" && $n ni $seen} { lappend seen $n; lappend q [list $n [concat $p $d]] }
    }
}
foreach d $path_to_cp { send_cmd $d; run_intcode }
foreach it $all_items { send_cmd "drop $it"; run_intcode }

set n [llength $all_items]
for {set i 0} {$i < (1 << $n)} {incr i} {
    for {set j 0} {$j < $n} {incr j} {
        if {($i >> $j) & 1} { send_cmd "take [lindex $all_items $j]"; run_intcode }
    }
    send_cmd $sensor_dir
    set res [run_intcode]
    if {[regexp {typing (\d+) on the keypad} $res -> code]} { puts $code; exit }
    for {set j 0} {$j < $n} {incr j} {
        if {($i >> $j) & 1} { send_cmd "drop [lindex $all_items $j]"; run_intcode }
    }
}
