
proc bfs {sy sx h w} {
    global grid unit_at
    set dists [dict create "$sy,$sx" 0]
    set queue [list $sy $sx 0]
    set head 0
    while {$head < [llength $queue]} {
        set cy [lindex $queue $head]; set cx [lindex $queue [expr {$head+1}]]; set d [lindex $queue [expr {$head+2}]]
        incr head 3
        set nd [expr {$d + 1}]
        foreach {dy dx} {-1 0 0 -1 0 1 1 0} {
            set ny [expr {$cy + $dy}]; set nx [expr {$cx + $dx}]
            if {$ny >= 0 && $ny < $h && $nx >= 0 && $nx < $w} {
                if {![dict exists $dists "$ny,$nx"] && $grid($ny,$nx) eq "." && ![info exists unit_at($ny,$nx)]} {
                    dict set dists "$ny,$nx" $nd
                    lappend queue $ny $nx $nd
                }
            }
        }
    }
    return $dists
}

proc simulate {elf_power lines initial_elves} {
    global units unit_at grid
    array unset units; array unset unit_at; array unset grid
    set h [llength $lines]; set w [string length [lindex $lines 0]]
    set uid_gen 0; set elves 0; set goblins 0
    for {set y 0} {$y < $h} {incr y} {
        set line [lindex $lines $y]
        for {set x 0} {$x < $w} {incr x} {
            set char [string index $line $x]
            if {$char eq "E" || $char eq "G"} {
                set id [incr uid_gen]; set units($id,y) $y; set units($id,x) $x
                set units($id,type) $char; set units($id,hp) 200
                set units($id,power) [expr {$char eq "E" ? $elf_power : 3}]
                set unit_at($y,$x) $id
                if {$char eq "E"} {incr elves} else {incr goblins}
                set grid($y,$x) "."
            } else { set grid($y,$x) $char }
        }
    }
    set rounds 0
    while {1} {
        set unit_ids {}
        foreach k [array names units "*,y"] {
            set id [lindex [split $k ","] 0]
            lappend unit_ids [format "%05d-%05d-%d" $units($id,y) $units($id,x) $id]
        }
        set sorted_unit_ids [lsort $unit_ids]
        foreach u_entry $sorted_unit_ids {
            set uid [lindex [split $u_entry "-"] 2]
            if {![info exists units($uid,hp)]} continue
            if {$goblins == 0 || $elves == 0} {
                set total_hp 0
                foreach k [array names units "*,hp"] { incr total_hp $units($k) }
                return [list [expr {$rounds * $total_hp}] 0]
            }
            set uy $units($uid,y); set ux $units($uid,x); set in_range 0
            foreach {dy dx} {-1 0 0 -1 0 1 1 0} {
                set ny [expr {$uy + $dy}]; set nx [expr {$ux + $dx}]
                if {[info exists unit_at($ny,$nx)] && $units($unit_at($ny,$nx),type) ne $units($uid,type)} {
                    set in_range 1; break
                }
            }
            if {!$in_range} {
                set dists [bfs $uy $ux $h $w]; set min_d 999999; set target_tiles {}
                foreach k [array names units "*,type"] {
                    set eid [lindex [split $k ","] 0]
                    if {$units($eid,type) ne $units($uid,type)} {
                        foreach {dy dx} {-1 0 0 -1 0 1 1 0} {
                            set ty [expr {$units($eid,y) + $dy}]; set tx [expr {$units($eid,x) + $dx}]
                            if {[dict exists $dists "$ty,$tx"]} {
                                set d [dict get $dists "$ty,$tx"]
                                if {$d < $min_d} {set min_d $d}; lappend target_tiles $ty $tx $d
                            }
                        }
                    }
                }
                if {$min_d < 999999} {
                    set candidates {}
                    foreach {ty tx td} $target_tiles { if {$td == $min_d} { lappend candidates [format "%05d-%05d" $ty $tx] } }
                    lassign [split [lindex [lsort $candidates] 0] "-"] ty tx
                    scan $ty %d ty; scan $tx %d tx
                    unset unit_at($uy,$ux); set step_dists [bfs $ty $tx $h $w]; set unit_at($uy,$ux) $uid
                    set best_sd 999999; set final_step ""
                    foreach {dy dx} {-1 0 0 -1 0 1 1 0} {
                        set ny [expr {$uy + $dy}]; set nx [expr {$ux + $dx}]
                        if {[dict exists $step_dists "$ny,$nx"]} {
                            set sd [dict get $step_dists "$ny,$nx"]
                            if {$sd < $best_sd} { set best_sd $sd; set final_step [list $ny $nx] }
                        }
                    }
                    if {$final_step ne ""} {
                        unset unit_at($uy,$ux); lassign $final_step uy ux
                        set units($uid,y) $uy; set units($uid,x) $ux; set unit_at($uy,$ux) $uid
                    }
                }
            }
            set target_id ""; set best_hp 999
            foreach {dy dx} {-1 0 0 -1 0 1 1 0} {
                set ny [expr {$uy + $dy}]; set nx [expr {$ux + $dx}]
                if {[info exists unit_at($ny,$nx)]} {
                    set eid $unit_at($ny,$nx)
                    if {$units($eid,type) ne $units($uid,type) && $units($eid,hp) < $best_hp} {
                        set best_hp $units($eid,hp); set target_id $eid
                    }
                }
            }
            if {$target_id ne ""} {
                incr units($target_id,hp) [expr {-$units($uid,power)}]
                if {$units($target_id,hp) <= 0} {
                    if {$units($target_id,type) eq "E"} { return [list 0 1] }
                    incr goblins -1; unset unit_at($units($target_id,y),$units($target_id,x)); array unset units "$target_id,*"
                }
            }
        }
        incr rounds
    }
}

set fp [open "input.txt" r]; set lines {}
while {[gets $fp line] >= 0} { if {[string trim $line] ne ""} { lappend lines $line } }
close $fp
set initial_elves 0
foreach line $lines { foreach c [split $line ""] { if {$c eq "E"} {incr initial_elves} } }
for {set p 4} {1} {incr p} {
    lassign [simulate $p $lines $initial_elves] outcome elf_died
    if {!$elf_died} { puts $outcome; break }
}
