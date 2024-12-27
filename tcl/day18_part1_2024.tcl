
#!/usr/bin/env tclsh

proc solve {filename} {
    set grid_size 71
    set max_bytes 1024
    set grid [lrepeat $grid_size [lrepeat $grid_size 0]]
    set start_x 0
    set start_y 0
    set end_x 70
    set end_y 70

    set file [open $filename r]
    set byte_count 0
    while {[gets $file line] != -1 && $byte_count < $max_bytes} {
        if {[regexp {^(\d+),(\d+)$} $line -> x y]} {
            set x [string trim $x]
            set y [string trim $y]
            lset grid $y $x 1
            incr byte_count
        }
    }
    close $file

    set queue [list [list $start_x $start_y 0]]
    set visited [dict create]
    dict set visited "$start_x,$start_y" 1

    while {[llength $queue] > 0} {
        set current [lindex $queue 0]
        set queue [lrange $queue 1 end]
        set x [lindex $current 0]
        set y [lindex $current 1]
        set steps [lindex $current 2]

        if {$x == $end_x && $y == $end_y} {
            return $steps
        }

        foreach {dx dy} {0 1 0 -1 1 0 -1 0} {
            set nx [expr {$x + $dx}]
            set ny [expr {$y + $dy}]

            if {$nx >= 0 && $nx < $grid_size && $ny >= 0 && $ny < $grid_size && [lindex [lindex $grid $ny] $nx] == 0} {
                if {![dict exists $visited "$nx,$ny"]} {
                    lappend queue [list $nx $ny [expr {$steps + 1}]]
                    dict set visited "$nx,$ny" 1
                }
            }
        }
    }
    return -1
}

set filename "input.txt"
set result [solve $filename]
puts $result
