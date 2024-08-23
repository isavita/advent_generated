proc readTargetArea {filename} {
    set file [open $filename r]
    set line [gets $file]
    close $file

    regexp {x=(\d+)\.\.(\d+), y=(-?\d+)\.\.(-?\d+)} $line -> x1 x2 y1 y2
    return [list $x1 $x2 $y1 $y2]
}

proc simulate {vx vy x1 x2 y1 y2} {
    set x 0
    set y 0
    set maxY 0

    while {$x <= $x2 && $y >= $y1} {
        set x [expr {$x + $vx}]
        set y [expr {$y + $vy}]
        set maxY [expr {max($maxY, $y)}]

        if {$vx > 0} {set vx [expr {$vx - 1}]}
        set vy [expr {$vy - 1}]

        if {$x >= $x1 && $x <= $x2 && $y >= $y1 && $y <= $y2} {
            return $maxY
        }
    }
    return 0
}

proc max {a b} {
    return [expr {$a > $b ? $a : $b}]
}

proc findHighestY {x1 x2 y1 y2} {
    set highestY 0

    for {set vx 0} {$vx <= $x2} {incr vx} {
        for {set vy 0} {$vy <= abs($y1)} {incr vy} {
            set maxY [simulate $vx $vy $x1 $x2 $y1 $y2]
            set highestY [expr {max($highestY, $maxY)}]
        }
    }
    return $highestY
}

set targetArea [readTargetArea "input.txt"]
set x1 [lindex $targetArea 0]
set x2 [lindex $targetArea 1]
set y1 [lindex $targetArea 2]
set y2 [lindex $targetArea 3]

puts [findHighestY $x1 $x2 $y1 $y2]