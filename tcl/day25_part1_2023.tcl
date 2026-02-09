
proc solve {} {
    set fp [open "input.txt" r]
    set data [read $fp]
    close $fp

    set id_map [dict create]
    set id_cnt 0
    set adj [dict create]
    set raw_edges [list]

    foreach line [split $data "\n"] {
        if {[string trim $line] eq ""} continue
        set parts [split $line ":"]
        set u_str [string trim [lindex $parts 0]]
        
        if {![dict exists $id_map $u_str]} {
            dict set id_map $u_str $id_cnt
            set u $id_cnt
            incr id_cnt
        } else {
            set u [dict get $id_map $u_str]
        }
        
        set neighbors [split [string trim [lindex $parts 1]] " "]
        foreach v_str $neighbors {
            if {[string trim $v_str] eq ""} continue
            if {![dict exists $id_map $v_str]} {
                dict set id_map $v_str $id_cnt
                set v $id_cnt
                incr id_cnt
            } else {
                set v [dict get $id_map $v_str]
            }
            if {$u < $v} { lappend raw_edges [list $u $v] } else { lappend raw_edges [list $v $u] }
        }
    }

    set unique_edges [lsort -unique $raw_edges]
    set all_edges [dict create]
    foreach e $unique_edges {
        set u [lindex $e 0]
        set v [lindex $e 1]
        dict set all_edges "$u:$v" 1
        dict lappend adj $u $v
        dict lappend adj $v $u
    }

    set N $id_cnt
    set source 0

    for {set target 1} {$target < $N} {incr target} {
        set active_edges $all_edges
        set paths_removed 0
        
        for {set k 0} {$k < 3} {incr k} {
            set queue [list $source]
            set head 0
            set visited [dict create $source 1]
            set parent [dict create]
            set found 0
            
            while {$head < [llength $queue]} {
                set curr [lindex $queue $head]
                incr head
                if {$curr == $target} { set found 1; break }
                
                foreach nb [dict get $adj $curr] {
                    if {[dict exists $visited $nb]} continue
                    if {$curr < $nb} { set key "$curr:$nb" } else { set key "$nb:$curr" }
                    if {[dict exists $active_edges $key]} {
                        dict set visited $nb 1
                        dict set parent $nb $curr
                        lappend queue $nb
                    }
                }
            }
            
            if {!$found} break
            
            set curr $target
            while {$curr != $source} {
                set prev [dict get $parent $curr]
                if {$prev < $curr} { set key "$prev:$curr" } else { set key "$curr:$prev" }
                dict unset active_edges $key
                set curr $prev
            }
            incr paths_removed
        }

        if {$paths_removed == 3} {
            set queue [list $source]
            set head 0
            set visited [dict create $source 1]
            while {$head < [llength $queue]} {
                set curr [lindex $queue $head]
                incr head
                foreach nb [dict get $adj $curr] {
                    if {[dict exists $visited $nb]} continue
                    if {$curr < $nb} { set key "$curr:$nb" } else { set key "$nb:$curr" }
                    if {[dict exists $active_edges $key]} {
                        dict set visited $nb 1
                        lappend queue $nb
                    }
                }
            }
            
            if {![dict exists $visited $target]} {
                puts [expr {[dict size $visited] * ($N - [dict size $visited])}]
                return
            }
        }
    }
}

solve
