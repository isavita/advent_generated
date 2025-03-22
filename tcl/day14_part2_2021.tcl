
proc solve {filename steps} {
    # Read input from file
    set fp [open $filename r]
    set lines [read $fp]
    close $fp
    set lines [split $lines "\n"]

    # Parse template and rules
    set template [lindex $lines 0]
    set rules {}
    foreach line [lrange $lines 2 end] {
        if {$line eq ""} continue
        regexp {([A-Z]+) -> ([A-Z])} $line _ pair insert
        dict set rules $pair $insert
    }

    # Initialize pair counts
    set pairs {}
    for {set i 0} {$i < [string length $template] - 1} {incr i} {
        set pair [string range $template $i $i+1]
        if {[dict exists $pairs $pair]} {
            dict incr pairs $pair
        } else {
            dict set pairs $pair 1
        }
    }

    # Simulate polymerization
    for {set step 0} {$step < $steps} {incr step} {
        set newPairs {}
        dict for {pair count} $pairs {
            if {[dict exists $rules $pair]} {
                set insert [dict get $rules $pair]
                set firstPair [string index $pair 0]$insert
                set secondPair $insert[string index $pair 1]
                if {[dict exists $newPairs $firstPair]} {
                    dict incr newPairs $firstPair $count
                } else {
                    dict set newPairs $firstPair $count
                }
                if {[dict exists $newPairs $secondPair]} {
                    dict incr newPairs $secondPair $count
                } else {
                    dict set newPairs $secondPair $count
                }
            } else {
                # if no rule, keep the same pair
                if {[dict exists $newPairs $pair]} {
                    dict incr $newPairs $pair $count
                } else {
                    dict set $newPairs $pair $count
                }
            }
        }
        set pairs $newPairs
    }

    # Count element occurrences
    set elementCounts {}
    foreach {pair count} [dict get $pairs] {
        foreach element [split $pair ""] {
            if {$element ne ""} {
                if {[dict exists $elementCounts $element]} {
                    dict incr elementCounts $element $count
                } else {
                    dict set elementCounts $element $count
                }
            }
        }
    }

    # Adjust counts for the first and last element
    set firstElement [string index $template 0]
    set lastElement [string index $template end]

    dict incr elementCounts $firstElement 1
    dict incr elementCounts $lastElement 1

    # Divide counts by 2 (since each element in pair is counted twice)
    set adjustedElementCounts {}
    dict for {element count} $elementCounts {
        dict set adjustedElementCounts $element [expr {$count / 2.0}]
    }

    # Find min and max counts
    set minCount Inf
    set maxCount -Inf
    dict for {element count} $adjustedElementCounts {
        if {$count < $minCount} {
            set minCount $count
        }
        if {$count > $maxCount} {
            set maxCount $count
        }
    }

    # Return the difference
    return [expr {$maxCount - $minCount}]
}

# Main entry point
set resultPart1 [solve "input.txt" 10]
puts "Part 1: $resultPart1"

set resultPart2 [solve "input.txt" 40]
puts "Part 2: $resultPart2"
