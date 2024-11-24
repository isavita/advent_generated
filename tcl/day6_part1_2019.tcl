
# Function to read the input file and build the orbit map
proc buildOrbitMap {filename} {
    set orbits [dict create]
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        lassign [split $line ")"] center satellite
        dict lappend orbits $center $satellite
    }
    close $file
    return $orbits
}

# Recursive function to count orbits
proc countOrbits {orbits center {depth 0}} {
    set count $depth
    if {[dict exists $orbits $center]} {
        foreach satellite [dict get $orbits $center] {
            incr count [countOrbits $orbits $satellite [expr {$depth + 1}]]
        }
    }
    return $count
}

# Main program
proc main {} {
    set filename "input.txt"
    set orbits [buildOrbitMap $filename]
    set totalOrbits [countOrbits $orbits "COM"]
    puts "Total number of direct and indirect orbits: $totalOrbits"
}

# Run the main program
main
