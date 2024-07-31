proc countDifferencesAndArrangements {filename} {
    set adapters [list]
    array set joltageDifferences {}

    # Read the input file and populate the adapters list
    set fileId [open $filename r]
    while {[gets $fileId line] >= 0} {
        lappend adapters $line
    }
    close $fileId
    
    # Sort adapters and add the built-in adapter rating
    set adapters [lsort -integer $adapters]
    lappend adapters [expr {[lindex $adapters end] + 3}]
    
    set previousJoltage 0
    foreach adapter $adapters {
        set difference [expr {$adapter - $previousJoltage}]
        if {[info exists joltageDifferences($difference)]} {
            set joltageDifferences($difference) [expr {$joltageDifferences($difference) + 1}]
        } else {
            set joltageDifferences($difference) 1
        }
        set previousJoltage $adapter
    }

    # Calculate the product of 1-jolt and 3-jolt differences
    set result [expr {$joltageDifferences(1) * $joltageDifferences(3)}]
    
    # Calculate distinct arrangements
    array set arrangements {}
    set arrangements(0) 1
    foreach adapter $adapters {
        set count 0
        for {set i 1} {$i <= 3} {incr i} {
            set prev [expr {$adapter - $i}]
            if {[info exists arrangements($prev)]} {
                set count [expr {$count + $arrangements($prev)}]
            }
        }
        set arrangements($adapter) $count
    }
    
    puts "Joltage Differences Product: $result"
    puts "Total Distinct Arrangements: $arrangements([lindex $adapters end])"
}

countDifferencesAndArrangements "input.txt"