proc readInput {filename} {
    set file [open $filename r]
    set line [gets $file]
    close $file
    regexp {x=(\d+)\.\.(\d+), y=(-?\d+)\.\.(-?\d+)} $line -> x1 x2 y1 y2
    return [list $x1 $x2 $y1 $y2]
}

proc simulate {vx vy x1 x2 y1 y2} {
    set x 0
    set y 0
    while {$x <= $x2 && $y >= $y1} {
        if {$x >= $x1 && $x <= $x2 && $y >= $y1 && $y <= $y2} {
            return 1
        }
        set x [expr {$x + $vx}]
        set y [expr {$y + $vy}]
        set vx [expr {$vx > 0 ? $vx - 1 : ($vx < 0 ? $vx + 1 : 0)}]
        set vy [expr {$vy - 1}]
    }
    return 0
}

proc countValidVelocities {x1 x2 y1 y2} {
    set count 0
    for {set vx 0} {$vx <= $x2} {incr vx} {
        for {set vy $y1} {$vy <= -$y1} {incr vy} {
            if {[simulate $vx $vy $x1 $x2 $y1 $y2]} {
                incr count
            }
        }
    }
    return $count
}

set targetArea [readInput "input.txt"]
set x1 [lindex $targetArea 0]
set x2 [lindex $targetArea 1]
set y1 [lindex $targetArea 2]
set y2 [lindex $targetArea 3]

set result [countValidVelocities $x1 $x2 $y1 $y2]
puts $result