
set dirs {-1 -1 -1 0 -1 1 0 1 1 1 1 0 1 -1 0 -1}
set order {1 5 7 3}

proc hash {x y} {expr {$x * 10000 + $y}}

proc check_neighbors {x y elf_map_name check_dirs} {
    global dirs
    upvar 1 $elf_map_name elf_map
    foreach dir_index $check_dirs {
        set dx [lindex $dirs [expr {$dir_index * 2}]]
        set dy [lindex $dirs [expr {$dir_index * 2 + 1}]]
        if {[info exists elf_map([hash [expr {$x + $dx}] [expr {$y + $dy}]])]} {
            return 1
        }
    }
    return 0
}

proc run {elves_name elf_map_name elves_list curr_dir} {
    global dirs order
    upvar 1 $elves_name elves
    upvar 1 $elf_map_name elf_map

    array set proposes {}
    array set elf_proposals {}

    foreach elf_id $elves_list {
        set x $elves($elf_id,x)
        set y $elves($elf_id,y)

        if {[check_neighbors $x $y elf_map {0 1 2 3 4 5 6 7}]} {
            set elves($elf_id,moving) 0
            for {set i 0} {$i < 4} {incr i} {
                set dir_index [lindex $order [expr {($curr_dir + $i) % 4}]]
                set check_dirs {}
                if {$dir_index == 1} {set check_dirs {0 1 2}}
                if {$dir_index == 5} {set check_dirs {4 5 6}}
                if {$dir_index == 7} {set check_dirs {6 7 0}}
                if {$dir_index == 3} {set check_dirs {2 3 4}}

                if {![check_neighbors $x $y elf_map $check_dirs]} {
                    set dx [lindex $dirs [expr {$dir_index * 2}]]
                    set dy [lindex $dirs [expr {$dir_index * 2 + 1}]]
                    set next_x [expr {$x + $dx}]
                    set next_y [expr {$y + $dy}]
                    set dest_hash [hash $next_x $next_y]

                    if {![info exists proposes($dest_hash)]} {set proposes($dest_hash) 0}
                    incr proposes($dest_hash)
                    set elf_proposals($elf_id) $dest_hash
                    set elves($elf_id,next_x) $next_x
                    set elves($elf_id,next_y) $next_y
                    set elves($elf_id,moving) 1
                    break
                }
            }
        } else {
            set elves($elf_id,moving) 0
        }
    }

    set moved_this_round 0
    foreach elf_id $elves_list {
        if {$elves($elf_id,moving)} {
            set next_hash $elf_proposals($elf_id)
             if {[info exists proposes($next_hash)] && $proposes($next_hash) == 1} {
                unset elf_map([hash $elves($elf_id,x) $elves($elf_id,y)])
                set elves($elf_id,x) $elves($elf_id,next_x)
                set elves($elf_id,y) $elves($elf_id,next_y)
                set elf_map([hash $elves($elf_id,x) $elves($elf_id,y)]) $elf_id
                set moved_this_round 1
            }
        }
        set elves($elf_id,moving) 0
    }

    return $moved_this_round
}

proc main {} {
    array set elves {}
    array set elf_map {}
    set elves_list {}

    set filename "input.txt"
    set file_id [open $filename r]
    set row 0
    set elf_id 0
    while {[gets $file_id line] != -1} {
        set chars [split $line ""]
        for {set col 0} {$col < [llength $chars]} {incr col} {
            if {[lindex $chars $col] eq "#"} {
                set elves($elf_id,x) $row
                set elves($elf_id,y) $col
                set elves($elf_id,moving) 0
                set elves($elf_id,next_x) 0
                set elves($elf_id,next_y) 0
                set elf_map([hash $row $col]) $elf_id
                lappend elves_list $elf_id
                incr elf_id
            }
        }
        incr row
    }
    close $file_id

    set round 0
    while {true} {
        set someone_moved [run elves elf_map $elves_list [expr {$round % 4}]]
        incr round
        if {!$someone_moved} {
            puts $round
            break
        }
    }
}

main
