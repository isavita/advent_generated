
proc gcd {a b} {
    set a [expr {abs($a)}]
    set b [expr {abs($b)}]
    while {$b != 0} {
        set temp $b
        set b [expr {$a % $b}]
        set a $temp
    }
    return $a
}

proc main {} {
    set f [open "input.txt" r]
    set grid {}
    while {[gets $f line] != -1} {
        lappend grid $line
    }
    close $f

    set h [llength $grid]
    if {$h == 0} {
        puts 0
        return
    }
    set w [string length [lindex $grid 0]]

    set antennas [dict create]
    for {set y 0} {$y < $h} {incr y} {
        set row [lindex $grid $y]
        for {set x 0} {$x < $w} {incr x} {
            set char [string index $row $x]
            if {$char ne "."} {
                dict lappend antennas $char [list $y $x]
            }
        }
    }

    set lines_per_freq [dict create]
    foreach freq [dict keys $antennas] {
        set coords [dict get $antennas $freq]
        set lines_set [dict create]
        set n [llength $coords]
        for {set i 0} {$i < $n} {incr i} {
            set A [lindex $coords $i]
            set Ay [lindex $A 0]
            set Ax [lindex $A 1]
            for {set j [expr {$i + 1}]} {$j < $n} {incr j} {
                set B [lindex $coords $j]
                set By [lindex $B 0]
                set Bx [lindex $B 1]

                set dy [expr {$By - $Ay}]
                set dx [expr {$Bx - $Ax}]

                if {$dy == 0 && $dx == 0} {
                    continue
                }

                set g [gcd $dy $dx]
                set sy [expr {$dy / $g}]
                set sx [expr {$dx / $g}]

                # Normalize direction vector (sx, sy)
                # Ensure sx is positive, or if sx is 0, ensure sy is positive
                if {$sx < 0 || ($sx == 0 && $sy < 0)} {
                    set sx [expr {-$sx}]
                    set sy [expr {-$sy}]
                }

                # Calculate constant c for sy * x - sx * y = c
                # Choose point A (Ax, Ay)
                set c [expr {$sy * $Ax - $sx * $Ay}]

                dict set lines_set "${sx},${sy},${c}" 1
            }
        }
        dict set lines_per_freq $freq $lines_set
    }

    set antinodes [dict create]
    foreach freq [dict keys $lines_per_freq] {
        set lines_set [dict get $lines_per_freq $freq]
        foreach line_key [dict keys $lines_set] {
            lassign [split $line_key ","] sx sy c
            set sx [expr {int($sx)}]
            set sy [expr {int($sy)}]
            set c [expr {int($c)}]

            if {$sx == 0 && $sy == 0} {
                continue
            }

            if {$sy == 0} { # Horizontal line equivalent in normalized space: -sx * y = c => sx * y = -c
                if {$sx != 0} {
                    set numerator [expr {-$c}]
                    if {[expr {$numerator % $sx}] == 0} {
                        set y_val [expr {$numerator / $sx}]
                        if {$y_val >= 0 && $y_val < $h} {
                            for {set x 0} {$x < $w} {incr x} {
                                dict set antinodes "${y_val},${x}" 1
                            }
                        }
                    }
                }
            } elseif {$sx == 0} { # Vertical line equivalent in normalized space: sy * x = c
                 if {$sy != 0} {
                    if {[expr {$c % $sy}] == 0} {
                        set x_val [expr {$c / $sy}]
                        if {$x_val >= 0 && $x_val < $w} {
                            for {set y 0} {$y < $h} {incr y} {
                                dict set antinodes "${y},${x_val}" 1
                            }
                        }
                    }
                 }
            } else { # General line: sy * x - sx * y = c => sy * x = c + sx * y
                for {set y 0} {$y < $h} {incr y} {
                    set val [expr {$c + $sx * $y}]
                    if {[expr {$val % $sy}] == 0} {
                        set x_val [expr {$val / $sy}]
                        if {$x_val >= 0 && $x_val < $w} {
                            dict set antinodes "${y},${x_val}" 1
                        }
                    }
                }
            }
        }
    }

    puts [dict size $antinodes]
}

main
