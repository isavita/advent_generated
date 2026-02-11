
set f [open "input.txt" r]
set data [read $f]
close $f
set letters [regexp -all -inline {[ABCD]} $data]

set s "..........."
append s [lindex $letters 0] "DD" [lindex $letters 4]
append s [lindex $letters 1] "CB" [lindex $letters 5]
append s [lindex $letters 2] "BA" [lindex $letters 6]
append s [lindex $letters 3] "AC" [lindex $letters 7]

set target "...........AAAABBBBCCCCDDDD"
array set r_data {A {11 12 13 14} B {15 16 17 18} C {19 20 21 22} D {23 24 25 26}}
array set r_col {A 3 B 5 C 7 D 9}
set h_stops {0 1 3 5 7 9 10}
array set energy {A 1 B 10 C 100 D 1000}

set pq [list [list 0 $s]]
set visited [dict create $s 0]

while {[llength $pq] > 0} {
    set pq [lsort -integer -index 0 $pq]
    set curr [lindex $pq 0]
    set pq [lrange $pq 1 end]
    lassign $curr d state
    
    if {$d > [dict get $visited $state]} continue
    if {$state eq $target} { puts $d; exit }
    
    set room_moves {}
    for {set i 0} {$i < 11} {incr i} {
        set char [string index $state $i]
        if {$char eq "."} continue
        set rd $r_data($char); set rc $r_col($char); set r_h_idx [expr {$rc - 1}]
        set target_spot -1; set can_enter 1
        foreach idx $rd {
            set occ [string index $state $idx]
            if {$occ eq "."} { set target_spot $idx } elseif {$occ ne $char} { set can_enter 0; break }
        }
        if {$target_spot == -1 || !$can_enter} continue
        set clear 1; set step [expr {$i < $r_h_idx ? 1 : -1}]
        for {set h [expr {$i + $step}]} {1} {incr h $step} {
            if {[string index $state $h] ne "."} { set clear 0; break }
            if {$h == $r_h_idx} break
        }
        if {!$clear} continue
        set dist [expr {abs(($i+1)-$rc) + ($target_spot - [lindex $rd 0] + 1)}]
        set new_d [expr {$d + $dist * $energy($char)}]
        set ns [string replace $state $i $i "."]; set ns [string replace $ns $target_spot $target_spot $char]
        if {![dict exists $visited $ns] || $new_d < [dict get $visited $ns]} {
            dict set visited $ns $new_d; lappend room_moves [list $new_d $ns]
        }
    }
    
    if {[llength $room_moves] > 0} {
        foreach m $room_moves { lappend pq $m }; continue
    }
    
    foreach rtype {A B C D} {
        set rd $r_data($rtype); set rc $r_col($rtype); set r_h_idx [expr {$rc - 1}]
        foreach r_idx $rd {
            set char [string index $state $r_idx]
            if {$char eq "."} continue
            set should_move 0
            if {$char ne $rtype} { set should_move 1 } else {
                foreach b_idx $rd {
                    if {$b_idx > $r_idx && [string index $state $b_idx] ne $rtype} { set should_move 1; break }
                }
            }
            if {!$should_move} continue
            set clear_top 1
            foreach a_idx $rd {
                if {$a_idx < $r_idx && [string index $state $a_idx] ne "."} { set clear_top 0; break }
            }
            if {!$clear_top} continue
            foreach h_idx $h_stops {
                set clear_h 1; set step [expr {$h_idx < $r_h_idx ? 1 : -1}]
                for {set h $h_idx} {1} {incr h $step} {
                    if {[string index $state $h] ne "."} { set clear_h 0; break }
                    if {$h == $r_h_idx} break
                }
                if {!$clear_h} continue
                set dist [expr {abs(($h_idx+1)-$rc) + ($r_idx - [lindex $rd 0] + 1)}]
                set new_d [expr {$d + $dist * $energy($char)}]
                set ns [string replace $state $r_idx $r_idx "."]; set ns [string replace $ns $h_idx $h_idx $char]
                if {![dict exists $visited $ns] || $new_d < [dict get $visited $ns]} {
                    dict set visited $ns $new_d; lappend pq [list $new_d $ns]
                }
            }
        }
    }
}
