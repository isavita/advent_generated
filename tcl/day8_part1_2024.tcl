
proc main {} {
    set fileId [open "input.txt" r]
    set inputData [read $fileId]
    close $fileId

    set lines [split [string trim $inputData] "\n"]
    set h [llength $lines]
    if {$h == 0} {
        puts 0
        return
    }
    set w [string length [lindex $lines 0]]
    if {$w == 0} {
        puts 0
        return
    }

    array set antennas {}
    for {set y 0} {$y < $h} {incr y} {
        set line [lindex $lines $y]
        for {set x 0} {$x < $w} {incr x} {
            set char [string index $line $x]
            if {$char ne "."} {
                lappend antennas($char) [list $y $x]
            }
        }
    }

    array set antinodes {}

    foreach char [array names antennas] {
        set coords $antennas($char)
        set n [llength $coords]

        for {set i 0} {$i < $n} {incr i} {
            set coord1 [lindex $coords $i]
            set y1 [lindex $coord1 0]
            set x1 [lindex $coord1 1]

            for {set j [expr {$i + 1}]} {$j < $n} {incr j} {
                set coord2 [lindex $coords $j]
                set y2 [lindex $coord2 0]
                set x2 [lindex $coord2 1]

                set p1_y [expr {2 * $y1 - $y2}]
                set p1_x [expr {2 * $x1 - $x2}]

                if {$p1_y >= 0 && $p1_y < $h && $p1_x >= 0 && $p1_x < $w} {
                    array set antinodes [list [format "%d,%d" $p1_y $p1_x] 1]
                }

                set p2_y [expr {2 * $y2 - $y1}]
                set p2_x [expr {2 * $x2 - $x1}]

                if {$p2_y >= 0 && $p2_y < $h && $p2_x >= 0 && $p2_x < $w} {
                    array set antinodes [list [format "%d,%d" $p2_y $p2_x] 1]
                }
            }
        }
    }

    puts [array size antinodes]
}

main
