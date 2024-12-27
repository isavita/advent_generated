
proc solve {filename} {
    set file [open $filename r]
    set grid [read $file]
    close $file

    set rows [split $grid "\n"]
    set numRows [llength $rows]
    set numCols [string length [lindex $rows 0]]

    set visibleCount 0
    set maxScenicScore 0

    for {set r 0} {$r < $numRows} {incr r} {
        for {set c 0} {$c < $numCols} {incr c} {
            set height [string index [lindex $rows $r] $c]

            # Part 1: Visibility check
            set visible 0
            if {$r == 0 || $r == [expr {$numRows - 1}] || $c == 0 || $c == [expr {$numCols - 1}]} {
                set visible 1
            } else {
                # Check left
                set leftVisible 1
                for {set i 0} {$i < $c} {incr i} {
                    if {[string index [lindex $rows $r] $i] >= $height} {
                        set leftVisible 0
                        break
                    }
                }
                if {$leftVisible} {set visible 1}

                # Check right
                set rightVisible 1
                for {set i [expr {$c + 1}]} {$i < $numCols} {incr i} {
                    if {[string index [lindex $rows $r] $i] >= $height} {
                        set rightVisible 0
                        break
                    }
                }
                if {$rightVisible} {set visible 1}

                # Check up
                set upVisible 1
                for {set i 0} {$i < $r} {incr i} {
                    if {[string index [lindex $rows $i] $c] >= $height} {
                        set upVisible 0
                        break
                    }
                }
                if {$upVisible} {set visible 1}

                # Check down
                set downVisible 1
                for {set i [expr {$r + 1}]} {$i < $numRows} {incr i} {
                    if {[string index [lindex $rows $i] $c] >= $height} {
                        set downVisible 0
                        break
                    }
                }
                if {$downVisible} {set visible 1}
            }
            if {$visible} {incr visibleCount}

            # Part 2: Scenic score calculation
            set upScore 0
            for {set i [expr {$r - 1}]} {$i >= 0} {incr i -1} {
                incr upScore
                if {[string index [lindex $rows $i] $c] >= $height} {
                    break
                }
            }

            set downScore 0
            for {set i [expr {$r + 1}]} {$i < $numRows} {incr i} {
                incr downScore
                if {[string index [lindex $rows $i] $c] >= $height} {
                    break
                }
            }

            set leftScore 0
            for {set i [expr {$c - 1}]} {$i >= 0} {incr i -1} {
                incr leftScore
                if {[string index [lindex $rows $r] $i] >= $height} {
                    break
                }
            }

            set rightScore 0
            for {set i [expr {$c + 1}]} {$i < $numCols} {incr i} {
                incr rightScore
                if {[string index [lindex $rows $r] $i] >= $height} {
                    break
                }
            }

            set scenicScore [expr {$upScore * $downScore * $leftScore * $rightScore}]
            if {$scenicScore > $maxScenicScore} {
                set maxScenicScore $scenicScore
            }
        }
    }

    puts "Visible trees: $visibleCount"
    puts "Max scenic score: $maxScenicScore"
}

solve "input.txt"
