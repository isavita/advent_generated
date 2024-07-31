proc calculate_distance {instructions} {
    set direction 0
    set x 0
    set y 0

    foreach instruction $instructions {
        set turn [string index $instruction 0]
        set distance_str [string range $instruction 1 end]
        set distance [expr {$distance_str ne "" ? $distance_str + 0 : 0}]

        if {$turn == "R"} {
            set direction [expr {($direction + 1) % 4}]
        } elseif {$turn == "L"} {
            set direction [expr {($direction + 3) % 4}]
        }

        switch $direction {
            0 {set y [expr {$y + $distance}]}  ;# North
            1 {set x [expr {$x + $distance}]}  ;# East
            2 {set y [expr {$y - $distance}]}  ;# South
            3 {set x [expr {$x - $distance}]}  ;# West
        }
    }

    return [expr {abs($x) + abs($y)}]
}

set file [open "input.txt" r]
set line [read $file]
close $file

set instructions [split $line ", "]
puts [calculate_distance $instructions]