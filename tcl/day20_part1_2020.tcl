
proc readTiles {filename} {
    set tiles {}
    set id 0
    set tileData {}
    set f [open $filename r]
    while {[gets $f line] >= 0} {
        if {[string match "Tile *" $line]} {
            scan $line "Tile %d:" id
        } elseif {[string length $line] == 0} {
            if {[llength $tileData] > 0} {
                lappend tiles [list id $id data $tileData]
                set tileData {}
            }
        } else {
            lappend tileData $line
        }
    }
    if {[llength $tileData] > 0} {
        lappend tiles [list id $id data $tileData]
    }
    close $f
    return $tiles
}

proc calculateBorders {tile} {
    set data [dict get $tile data]
    set n [llength $data]

    set top [lindex $data 0]
    set bottom [lindex $data [expr {$n - 1}]]
    set left ""
    set right ""
    for {set i 0} {$i < $n} {incr i} {
        append left [string index [lindex $data $i] 0]
        append right [string index [lindex $data $i] [expr {$n - 1}]]
    }

    return [list $top $right $bottom $left [string reverse $top] [string reverse $right] [string reverse $bottom] [string reverse $left]]
}

proc buildBorderMap {tiles} {
    set borderMap [dict create]
    foreach tile $tiles {
        set id [dict get $tile id]
        set borders [calculateBorders $tile]
        foreach border $borders {
            if {[dict exists $borderMap $border]} {
                dict lappend borderMap $border $id
            } else {
                dict set borderMap $border [list $id]
            }
        }
    }
    return $borderMap
}

proc findCornerProduct {tiles borderMap} {
    set product 1
    foreach tile $tiles {
        set id [dict get $tile id]
        set borders [calculateBorders $tile]
        set uniqueBorders 0
        for {set i 0} {$i < 4} {incr i} {
            set border [lindex $borders $i]
            if {[llength [dict get $borderMap $border]] == 1} {
                incr uniqueBorders
            }
        }
        if {$uniqueBorders == 2} {
            set product [expr {$product * $id}]
        }
    }
    return $product
}

proc main {} {
    set tiles [readTiles "input.txt"]
    set borderMap [buildBorderMap $tiles]
    set cornerProduct [findCornerProduct $tiles $borderMap]
    puts $cornerProduct
}

# Main entry point
main
