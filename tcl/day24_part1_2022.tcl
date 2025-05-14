
proc gcd {a b} {
    while {$b != 0} {
        set temp $b
        set b [expr {$a % $b}]
        set a $temp
    }
    return $a
}

proc lcm {a b} {
    if {$a == 0 || $b == 0} {
         return 1
    }
    expr {($a * $b) / [gcd $a $b]}
}

proc isBlizzard {grid row col time inner_rows inner_cols} {
    set r $inner_rows
    set c $inner_cols
    set target_inner_row [expr {$row - 1}]
    set target_inner_col [expr {$col - 1}]

    set start_inner_col_right [expr {(($target_inner_col - $time) % $c + $c) % $c}]
    if {[lindex [lindex $grid $row] [expr {$start_inner_col_right + 1}]] eq ">"} { return 1 }

    set start_inner_col_left [expr {(($target_inner_col + $time) % $c + $c) % $c}]
    if {[lindex [lindex $grid $row] [expr {$start_inner_col_left + 1}]] eq "<"} { return 1 }

    set start_inner_row_down [expr {(($target_inner_row - $time) % $r + $r) % $r}]
    if {[lindex [lindex $grid [expr {$start_inner_row_down + 1}]] $col] eq "v"} { return 1 }

    set start_inner_row_up [expr {(($target_inner_row + $time) % $r + $r) % $r}]
    if {[lindex [lindex $grid [expr {$start_inner_row_up + 1}]] $col] eq "^"} { return 1 }

    return 0
}

proc main {} {
    set filename "input.txt"
    set fd [open $filename r]
    set data [read $fd]
    close $fd

    set lines [split [string trim $data] "\n"]
    set grid [list]
    foreach line $lines {
        lappend grid [split $line ""]
    }

    set rows [llength $grid]
    set cols [llength [lindex $grid 0]]

    set startRow 0
    set startCol -1
    for {set c 0} {$c < $cols} {incr c} {
        if {[lindex [lindex $grid $startRow] $c] eq "."} {
            set startCol $c
            break
        }
    }

    set endRow [expr {$rows - 1}]
    set endCol -1
    for {set c 0} {$c < $cols} {incr c} {
        if {[lindex [lindex $grid $endRow] $c] eq "."} {
            set endCol $c
            break
        }
    }

    set inner_rows [expr {$rows - 2}]
    set inner_cols [expr {$cols - 2}]
    set blizzard_lcm [lcm $inner_rows $inner_cols]

    set queue [list [list $startRow $startCol 0]]
    array set visited {"$startRow $startCol 0" 1}

    set dr {0 0 1 -1 0}
    set dc {1 -1 0 0 0}

    while {[llength $queue] > 0} {
        set current [lindex $queue 0]
        set queue [lrange $queue 1 end]

        set curRow [lindex $current 0]
        set curCol [lindex $current 1]
        set curTime [lindex $current 2]

        if {$curRow == $endRow && $curCol == $endCol} {
            puts $curTime
            return
        }

        for {set i 0} {$i < 5} {incr i} {
            set newRow [expr {$curRow + [lindex $dr $i]}]
            set newCol [expr {$curCol + [lindex $dc $i]}]
            set newTime [expr {$curTime + 1}]

            if {$newRow >= 0 && $newRow < $rows && $newCol >= 0 && $newCol < $cols && [lindex [lindex $grid $newRow] $newCol] ne "#"} {
                set is_bliz 0
                if {$newRow > 0 && $newRow < $endRow && $newCol > 0 && $newCol < $endCol} {
                     if {[isBlizzard $grid $newRow $newCol $newTime $inner_rows $inner_cols]} {
                         set is_bliz 1
                     }
                }

                if {!$is_bliz} {
                    set visited_key "$newRow $newCol [expr {$newTime % $blizzard_lcm}]"
                    if {![info exists visited($visited_key)]} {
                        set visited($visited_key) 1
                        lappend queue [list $newRow $newCol $newTime]
                    }
                }
            }
        }
    }
    puts "-1"
}

main
