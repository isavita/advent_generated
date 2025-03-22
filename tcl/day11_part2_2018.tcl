
# Function to calculate the power level of a single fuel cell
proc calculatePowerLevel {x y serialNumber} {
    set rackID [expr {$x + 10}]
    set powerLevel [expr {$rackID * $y}]
    set powerLevel [expr {$powerLevel + $serialNumber}]
    set powerLevel [expr {$powerLevel * $rackID}]
    set powerLevel [expr {($powerLevel / 100) % 10}]
    set powerLevel [expr {$powerLevel - 5}]
    return $powerLevel
}

# Function to calculate the total power of a square of a given size
proc calculateSquarePower {grid x y size} {
    set totalPower 0
    for {set i 0} {$i < $size} {incr i} {
        for {set j 0} {$j < $size} {incr j} {
            set power [lindex $grid [expr {($y + $i -1) * 300 + ($x + $j - 1)}]]
            set totalPower [expr {$totalPower + $power}]
        }
    }
    return $totalPower
}

# Function to solve part 1
proc solvePart1 {serialNumber} {
    set grid {}
    for {set y 1} {$y <= 300} {incr y} {
        for {set x 1} {$x <= 300} {incr x} {
            lappend grid [calculatePowerLevel $x $y $serialNumber]
        }
    }

    set maxPower -1000000 ;# Initialize with a very small value
    set maxX 0
    set maxY 0

    for {set y 1} {$y <= 298} {incr y} {
        for {set x 1} {$x <= 298} {incr x} {
            set currentPower [calculateSquarePower $grid $x $y 3]
            if {$currentPower > $maxPower} {
                set maxPower $currentPower
                set maxX $x
                set maxY $y
            }
        }
    }
    return "$maxX,$maxY"
}


# Function to solve part 2
proc solvePart2 {serialNumber} {
    set grid {}
    for {set y 1} {$y <= 300} {incr y} {
        for {set x 1} {$x <= 300} {incr x} {
            lappend grid [calculatePowerLevel $x $y $serialNumber]
        }
    }
    
    # Precompute summed area table
    set summedAreaTable [list]
    for {set y 0} {$y <= 300} {incr y} {
        set row [list]
        for {set x 0} {$x <= 300} {incr x} {
            if {$x == 0 || $y == 0} {
                lappend row 0
            } else {
                set power [lindex $grid [expr {($y-1) * 300 + ($x-1)}]]
                set sum [expr {$power + [lindex [lindex $summedAreaTable [expr {$y - 1}]] $x] + [lindex $row [expr {$x - 1}]] - [lindex [lindex $summedAreaTable [expr {$y - 1}]] [expr {$x - 1}]]}]
                lappend row $sum
            }
        }
        lappend summedAreaTable $row
    }
    
    set maxPower -1000000
    set maxX 0
    set maxY 0
    set maxSize 0

     for {set size 1} {$size <= 300} {incr size} {
        for {set y 1} {$y <= [expr {301 - $size}]} {incr y} {
            for {set x 1} {$x <= [expr {301 - $size}]} {incr x} {
                # Calculate total power using summed area table
                set totalPower [expr {
                    [lindex [lindex $summedAreaTable [expr {$y + $size - 1}]] [expr {$x + $size - 1}]] -
                    [lindex [lindex $summedAreaTable [expr {$y - 1}]] [expr {$x + $size - 1}]] -
                    [lindex [lindex $summedAreaTable [expr {$y + $size - 1}]] [expr {$x - 1}]] +
                    [lindex [lindex $summedAreaTable [expr {$y - 1}]] [expr {$x - 1}]]
                }]

                if {$totalPower > $maxPower} {
                    set maxPower $totalPower
                    set maxX $x
                    set maxY $y
                    set maxSize $size
                }
            }
        }
    }

    return "$maxX,$maxY,$maxSize"
}

# Main function to read input and print results
proc main {} {
    if {[file exists "input.txt"]} {
        set f [open "input.txt" r]
        set serialNumber [string trim [read $f]]
        close $f

        # Ensure serialNumber is a valid integer
        if {[string is integer -strict $serialNumber]} {
             puts "Part 1: [solvePart1 $serialNumber]"
             puts "Part 2: [solvePart2 $serialNumber]"
        } else {
            puts "Error: Invalid serial number in input.txt"
        }
    } else {
        puts "Error: input.txt not found."
    }
}

# Call the main function
main
