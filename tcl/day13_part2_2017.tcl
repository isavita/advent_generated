
proc solve {filename} {
    set file [open $filename r]
    set layers {}
    while {[gets $file line] >= 0} {
        regexp {(\d+): (\d+)} $line _ depth range
        lappend layers [list $depth $range]
    }
    close $file

    # Part 1
    set severity 0
    foreach layer $layers {
        lassign $layer depth range
        if {([expr {$depth % (2 * ($range - 1))}] == 0)} {
            set severity [expr {$severity + $depth * $range}]
        }
    }
    puts "Part 1: $severity"

    # Part 2
    set delay 0
    while {1} {
        set caught 0
        foreach layer $layers {
            lassign $layer depth range
            if {([expr {($depth + $delay) % (2 * ($range - 1))}] == 0)} {
                set caught 1
                break
            }
        }
        if {!$caught} {
            puts "Part 2: $delay"
            break
        }
        incr delay
    }
}

solve "input.txt"
