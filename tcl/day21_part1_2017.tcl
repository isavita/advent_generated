proc rotate {input} {
    set parts [split $input /]
    set size [llength $parts]
    set newParts [list]
    for {set x 0} {$x < $size} {incr x} {
        set newRow {}
        for {set y [expr {$size - 1}]} {$y >= 0} {incr y -1} {
            append newRow [string index [lindex $parts $y] $x]
        }
        lappend newParts $newRow
    }
    return [join $newParts /]
}

proc flip {input} {
    set parts [split $input /]
    set newParts [list]
    foreach part $parts {
        set s $part
        set r {}
        for {set i [expr {[string length $s] - 1}]} {$i >= 0} {incr i -1} {
            append r [string index $s $i]
        }
        lappend newParts $r
    }
    return [join $newParts /]
}

proc enhance {input rules} {
    set current $input
    for {set i 0} {$i < 4} {incr i} {
        if {[dict exists $rules $current]} {
            return [dict get $rules $current]
        }
        set current [rotate $current]
    }
    
    set current [flip $input]
    
    for {set i 0} {$i < 4} {incr i} {
        if {[dict exists $rules $current]} {
            return [dict get $rules $current]
        }
        set current [rotate $current]
    }
    return ""
}

proc iterate {gridStr rules} {
    set grid [split $gridStr /]
    set N [llength $grid]

    if {[expr {$N % 2}] == 0} {
        set subSize 2
        set factor 3
    } else {
        set subSize 3
        set factor 4
    }
    
    set blocks [expr {$N / $subSize}]
    set newN [expr {$blocks * $factor}]

    set newGrid [list]
    for {set k 0} {$k < $newN} {incr k} {
        lappend newGrid ""
    }

    set subSize_1 [expr {$subSize - 1}]
    
    for {set by 0} {$by < $N} {set by [expr {$by + $subSize}]} {
        set targetYStart [expr {($by / $subSize) * $factor}]

        for {set bx 0} {$bx < $N} {set bx [expr {$bx + $subSize}]} {
            
            set squareRows [list]
            for {set dy 0} {$dy < $subSize} {incr dy} {
                set row [lindex $grid [expr {$by + $dy}]]
                lappend squareRows [string range $row $bx [expr {$bx + $subSize_1}]]
            }
            set square [join $squareRows /]
            
            set newRows [split [enhance $square $rules] /]
            
            for {set dy 0} {$dy < $factor} {incr dy} {
                set targetY [expr {$targetYStart + $dy}]
                lset newGrid $targetY [lindex $newGrid $targetY][lindex $newRows $dy]
            }
        }
    }
    
    return [join $newGrid /]
}

proc main {} {
    set rules [dict create]
    set fid [open "input.txt" r]
    while {[gets $fid line] != -1} {
        if {[string match *=>* $line]} {
            regexp {([^ ]+) => (.+)} $line _ pattern result
            dict set rules $pattern $result
        }
    }
    close $fid

    set grid {.#./..#/###}
    
    for {set i 0} {$i < 5} {incr i} {
        set grid [iterate $grid $rules]
    }
    
    set mapList {. {} / {}}
    set count [string length [string map $mapList $grid]]
    puts $count
}

main