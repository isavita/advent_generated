
#!/usr/bin/tclsh

set SIDE 5
set SQUARE [expr {$SIDE * $SIDE}]
set CENTER 12
set MINUTES 200

proc infested {space level cell} {
    if {[dict exists $space $level]} {
        set grid [dict get $space $level]
        return [lindex $grid $cell]
    }
    return 0
}

proc parse {} {
    set filename "input.txt"
    set fileId [open $filename r]
    set content [read $fileId]
    close $fileId

    set lines [split [string trim $content] "\n"]
    set res [lrepeat $::SQUARE 0]

    set row 0
    foreach line $lines {
        set line [string trim $line]
        for {set col 0} {$col < $::SIDE} {incr col} {
            if {[string index $line $col] eq "#"} {
                lset res [expr {$row * $::SIDE + $col}] 1
            }
        }
        incr row
    }
    return $res
}

proc next2 {space} {
    set newSpace [dict create]

    set levels [lsort -integer [dict keys $space]]
    set minLevel 0
    set maxLevel 0
    if {[llength $levels] > 0} {
        set minLevel [lindex $levels 0]
        set maxLevel [lindex $levels end]
    }


    for {set level [expr {$minLevel - 1}]} {$level <= [expr {$maxLevel + 1}]} {incr level} {
        set newGrid [lrepeat $::SQUARE 0]

        for {set cell 0} {$cell < $::SQUARE} {incr cell} {
            if {$cell == $::CENTER} {
                continue
            }

            set row [expr {$cell / $::SIDE}]
            set col [expr {$cell % $::SIDE}]
            set neighbours 0

            if {$row == 0} {
                if {[infested $space [expr {$level - 1}] 7]} {incr neighbours}
            }
            if {$col == 0} {
                if {[infested $space [expr {$level - 1}] 11]} {incr neighbours}
            }
            if {$col == [expr {$::SIDE - 1}]} {
                if {[infested $space [expr {$level - 1}] 13]} {incr neighbours}
            }
            if {$row == [expr {$::SIDE - 1}]} {
                if {[infested $space [expr {$level - 1}] 17]} {incr neighbours}
            }

            if {$cell == 7} {
                for {set i 0} {$i < $::SIDE} {incr i} {
                    if {[infested $space [expr {$level + 1}] $i]} {incr neighbours}
                }
            }
            if {$cell == 11} {
                for {set i 0} {$i < $::SIDE} {incr i} {
                    if {[infested $space [expr {$level + 1}] [expr {$::SIDE * $i}]]} {incr neighbours}
                }
            }
            if {$cell == 13} {
                for {set i 0} {$i < $::SIDE} {incr i} {
                    if {[infested $space [expr {$level + 1}] [expr {$::SIDE * $i + $::SIDE - 1}]]} {incr neighbours}
                }
            }
            if {$cell == 17} {
                for {set i 0} {$i < $::SIDE} {incr i} {
                    if {[infested $space [expr {$level + 1}] [expr {($::SIDE - 1) * $::SIDE + $i}]]} {incr neighbours}
                }
            }

            if {$row > 0 && $cell != 17} {
                if {[infested $space $level [expr {$cell - $::SIDE}]]} {incr neighbours}
            }
            if {$col > 0 && $cell != 13} {
                if {[infested $space $level [expr {$cell - 1}]]} {incr neighbours}
            }
            if {$col < [expr {$::SIDE - 1}] && $cell != 11} {
                if {[infested $space $level [expr {$cell + 1}]]} {incr neighbours}
                }
            if {$row < [expr {$::SIDE - 1}] && $cell != 7} {
                if {[infested $space $level [expr {$cell + $::SIDE}]]} {incr neighbours}
            }

            set current_infested [infested $space $level $cell]

            if {$current_infested == 1 && $neighbours != 1} {
                lset newGrid $cell 0
            } elseif {$current_infested == 0 && ($neighbours == 1 || $neighbours == 2)} {
                lset newGrid $cell 1
            } else {
                lset newGrid $cell $current_infested
            }
        }
        dict set newSpace $level $newGrid
    }

    clean newSpace
    return $newSpace
}

proc clean {spaceName} {
    upvar 1 $spaceName space_dict

    set levels [lsort -integer [dict keys $space_dict]]
    if {[llength $levels] == 0} { return }

    set minLevel [lindex $levels 0]
    set maxLevel [lindex $levels end]

    set countMin 0
    if {[dict exists $space_dict $minLevel]} {
        set gridMin [dict get $space_dict $minLevel]
        foreach cell_state $gridMin {
            if {$cell_state == 1} { incr countMin }
        }
    }

    set countMax 0
    if {[dict exists $space_dict $maxLevel]} {
        set gridMax [dict get $space_dict $maxLevel]
        foreach cell_state $gridMax {
            if {$cell_state == 1} { incr countMax }
        }
    }

    if {$countMin == 0 && [dict exists $space_dict $minLevel]} {
        dict unset space_dict $minLevel
    }

    set levels [lsort -integer [dict keys $space_dict]]
     if {[llength $levels] == 0} { return }
    set maxLevel [lindex $levels end]

    if {$countMax == 0 && [dict exists $space_dict $maxLevel]} {
        dict unset space_dict $maxLevel
    }
}

if {[info procs main] eq ""} {
    proc main {} {
        set initialGrid [parse]
        set space [dict create 0 $initialGrid]

        for {set i 0} {$i < $::MINUTES} {incr i} {
            set space [next2 $space]
        }

        set totalCount 0
        dict for {level grid} $space {
            foreach cell_state $grid {
                if {$cell_state == 1} {
                    incr totalCount
                }
            }
        }

        puts $totalCount
    }
}

main
