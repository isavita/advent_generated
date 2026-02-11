
proc get_mem {id addr} {
    global mem
    if {[info exists mem($id,$addr)]} { return $mem($id,$addr) }
    return 0
}

proc run_comp {id} {
    global mem ip rb inputs outputs halted comp_idle
    set comp_idle($id) 0
    while {1} {
        set ip_now $ip($id)
        set instr [get_mem $id $ip_now]
        set op [expr {$instr % 100}]
        set modes [expr {$instr / 100}]
        set m1 [expr {$modes % 10}]
        set m2 [expr {($modes / 10) % 10}]
        set m3 [expr {($modes / 100) % 10}]

        if {$op == 99} { set halted($id) 1; return }

        set p1a [expr {$ip_now + 1}]
        set v1 [get_mem $id $p1a]
        if {$m1 == 0} { set a1 $v1 } elseif {$m1 == 1} { set a1 $p1a } else { set a1 [expr {$rb($id) + $v1}] }

        if {$op == 1 || $op == 2 || $op == 5 || $op == 6 || $op == 7 || $op == 8} {
            set p2a [expr {$ip_now + 2}]
            set v2 [get_mem $id $p2a]
            if {$m2 == 0} { set a2 $v2 } elseif {$m2 == 1} { set a2 $p2a } else { set a2 [expr {$rb($id) + $v2}] }
        }
        if {$op == 1 || $op == 2 || $op == 7 || $op == 8} {
            set p3a [expr {$ip_now + 3}]
            set v3 [get_mem $id $p3a]
            if {$m3 == 0} { set a3 $v3 } else { set a3 [expr {$rb($id) + $v3}] }
        }

        switch $op {
            1 { set mem($id,$a3) [expr {[get_mem $id $a1] + [get_mem $id $a2]}]; incr ip($id) 4 }
            2 { set mem($id,$a3) [expr {[get_mem $id $a1] * [get_mem $id $a2]}]; incr ip($id) 4 }
            3 {
                if {[llength $inputs($id)] == 0} {
                    set mem($id,$a1) -1; incr ip($id) 2; set comp_idle($id) 1; return
                }
                set inputs($id) [lassign $inputs($id) val]
                set mem($id,$a1) $val
                incr ip($id) 2
            }
            4 { lappend outputs($id) [get_mem $id $a1]; incr ip($id) 2; if {[llength $outputs($id)] == 3} return }
            5 { if {[get_mem $id $a1] != 0} {set ip($id) [get_mem $id $a2]} else {incr ip($id) 3} }
            6 { if {[get_mem $id $a1] == 0} {set ip($id) [get_mem $id $a2]} else {incr ip($id) 3} }
            7 { set mem($id,$a3) [expr {[get_mem $id $a1] < [get_mem $id $a2] ? 1 : 0}]; incr ip($id) 4 }
            8 { set mem($id,$a3) [expr {[get_mem $id $a1] == [get_mem $id $a2] ? 1 : 0}]; incr ip($id) 4 }
            9 { incr rb($id) [get_mem $id $a1]; incr ip($id) 2 }
        }
    }
}

set f [open "input.txt" r]
set program [split [string trim [read $f]] ,]
close $f

for {set i 0} {$i < 50} {incr i} {
    set ip($i) 0
    set rb($i) 0
    set halted($i) 0
    set inputs($i) [list $i]
    set outputs($i) {}
    set comp_idle($i) 0
    set packet_queues($i) {}
    set addr 0
    foreach val $program {
        set mem($i,$addr) $val
        incr addr
    }
}

set nat_exists 0
set last_nat_y ""
while {1} {
    set all_idle 1
    for {set i 0} {$i < 50} {incr i} {
        if {[llength $packet_queues($i)] > 0} {
            set packet_queues($i) [lassign $packet_queues($i) pkt]
            lappend inputs($i) {*}$pkt
        }
        run_comp $i
        while {[llength $outputs($i)] >= 3} {
            set outputs($i) [lassign $outputs($i) dest x y]
            if {$dest == 255} {
                set nat_x $x; set nat_y $y; set nat_exists 1
            } elseif {$dest >= 0 && $dest < 50} {
                lappend packet_queues($dest) [list $x $y]
            }
        }
        if {[llength $packet_queues($i)] > 0 || !$comp_idle($i)} { set all_idle 0 }
    }
    if {$all_idle && $nat_exists} {
        if {$nat_y == $last_nat_y} { puts $nat_y; exit }
        set last_nat_y $nat_y
        lappend packet_queues(0) [list $nat_x $nat_y]
    }
}

