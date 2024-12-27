
proc solve {filename part} {
    set f [open $filename r]
    set instructions [read $f]
    close $f

    set ship_x 0
    set ship_y 0
    set waypoint_x 10
    set waypoint_y 1
    set direction 0 ;# 0:E, 90:S, 180:W, 270:N

    foreach line [split $instructions \n] {
        if {$line eq ""} continue
        regexp {([NSEWLRF])(\d+)} $line _ action value
        
        switch $action {
            N {
                if {$part == 1} {
                    incr ship_y $value
                } else {
                    incr waypoint_y $value
                }
            }
            S {
                if {$part == 1} {
                    incr ship_y -$value
                } else {
                    incr waypoint_y -$value
                }
            }
            E {
                if {$part == 1} {
                    incr ship_x $value
                } else {
                    incr waypoint_x $value
                }
            }
            W {
                if {$part == 1} {
                    incr ship_x -$value
                } else {
                    incr waypoint_x -$value
                }
            }
            L {
                if {$part == 1} {
                    set direction [expr {($direction - $value) % 360}]
                    if {$direction < 0} {
                        set direction [expr {$direction + 360}]
                    }
                } else {
                    for {set i 0} {$i < $value/90} {incr i} {
                        set tmp $waypoint_x
                        set waypoint_x [expr {-$waypoint_y}]
                        set waypoint_y $tmp
                    }
                }
            }
            R {
                if {$part == 1} {
                    set direction [expr {($direction + $value) % 360}]
                } else {
                    for {set i 0} {$i < $value/90} {incr i} {
                        set tmp $waypoint_x
                        set waypoint_x $waypoint_y
                        set waypoint_y [expr {-$tmp}]
                    }
                }
            }
            F {
                if {$part == 1} {
                    switch $direction {
                        0 { incr ship_x $value }
                        90 { incr ship_y -$value }
                        180 { incr ship_x -$value }
                        270 { incr ship_y $value }
                    }
                } else {
                    incr ship_x [expr {$waypoint_x * $value}]
                    incr ship_y [expr {$waypoint_y * $value}]
                }
            }
        }
    }
    return [expr {abs($ship_x) + abs($ship_y)}]
}

puts "Part 1: [solve input.txt 1]"
puts "Part 2: [solve input.txt 2]"
