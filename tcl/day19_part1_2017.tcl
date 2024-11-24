
#!/usr/bin/env tclsh

proc readGrid {filename} {
    set grid {}
    set file [open $filename r]
    while {[gets $file line] != -1} {
        lappend grid [split $line ""]
    }
    close $file
    return $grid
}

proc findPath {grid} {
    set x 0
    set y 0
    set letters {}
    
    for {set i 0} {$i < [llength [lindex $grid 0]]} {incr i} {
        if {[lindex [lindex $grid 0] $i] eq "|"} {
            set x $i
            break
        }
    }
    
    set dx 0
    set dy 1
    
    while {1} {
        if {$x < 0 || $x >= [llength [lindex $grid 0]] || $y < 0 || $y >= [llength $grid]} {
            break
        }
        
        set cell [lindex [lindex $grid $y] $x]
        
        if {$cell eq " "} {
            break
        }
        
        if {[regexp {[A-Z]} $cell]} {
            lappend letters $cell
        }
        
        if {$cell eq "+"} {
            if {$dx == 0} {
                if {$x > 0 && ([lindex [lindex $grid $y] [expr {$x-1}]] eq "-" || 
                    [regexp {[A-Z]} [lindex [lindex $grid $y] [expr {$x-1}]]])} {
                    set dx -1
                    set dy 0
                } else {
                    set dx 1
                    set dy 0
                }
            } else {
                if {$y > 0 && ([lindex [lindex $grid [expr {$y-1}]] $x] eq "|" || 
                    [regexp {[A-Z]} [lindex [lindex $grid [expr {$y-1}]] $x]])} {
                    set dx 0
                    set dy -1
                } else {
                    set dx 0
                    set dy 1
                }
            }
        }
        
        incr x $dx
        incr y $dy
    }
    
    return [join $letters ""]
}

set grid [readGrid "input.txt"]
puts [findPath $grid]
