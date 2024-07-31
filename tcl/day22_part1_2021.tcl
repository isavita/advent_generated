proc main {} {
    set grid [dict create]
    set inputFile [open "input.txt" r]
    set lines [split [read $inputFile] "\n"]
    close $inputFile

    foreach line $lines {
        if {[string length $line] == 0} continue
        regexp {(\w+) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)} $line match action x1 x2 y1 y2 z1 z2
        
        set x1 [expr {$x1 < -50 ? -50 : $x1}]
        set x2 [expr {$x2 > 50 ? 50 : $x2}]
        set y1 [expr {$y1 < -50 ? -50 : $y1}]
        set y2 [expr {$y2 > 50 ? 50 : $y2}]
        set z1 [expr {$z1 < -50 ? -50 : $z1}]
        set z2 [expr {$z2 > 50 ? 50 : $z2}]
        
        if {$x1 > $x2 || $y1 > $y2 || $z1 > $z2} continue
        
        for {set x $x1} {$x <= $x2} {incr x} {
            for {set y $y1} {$y <= $y2} {incr y} {
                for {set z $z1} {$z <= $z2} {incr z} {
                    set key "$x,$y,$z"
                    if {$action eq "on"} {
                        dict set grid $key 1
                    } else {
                        dict unset grid $key
                    }
                }
            }
        }
    }
    
    puts [dict size $grid]
}

main