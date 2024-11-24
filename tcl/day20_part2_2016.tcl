
# Function to read and parse the input file
proc readRanges {filename} {
    set ranges {}
    set fp [open $filename r]
    while {[gets $fp line] >= 0} {
        lassign [split $line "-"] start end
        lappend ranges [list $start $end]
    }
    close $fp
    return $ranges
}

# Function to find the lowest unblocked IP
proc findLowestUnblockedIP {ranges} {
    set ranges [lsort -integer -index 0 $ranges]
    set current 0

    foreach range $ranges {
        lassign $range start end
        if {$current < $start} {
            return $current
        }
        if {$end >= $current} {
            set current [expr {$end + 1}]
        }
    }
    return $current
}

# Function to count the number of allowed IPs
proc countAllowedIPs {ranges} {
    set ranges [lsort -integer -index 0 $ranges]
    set maxIP 4294967295
    set current 0
    set allowedCount 0

    foreach range $ranges {
        lassign $range start end
        if {$current < $start} {
            set allowedCount [expr {$allowedCount + ($start - $current)}]
        }
        if {$end >= $current} {
            set current [expr {$end + 1}]
        }
    }
    if {$current <= $maxIP} {
        set allowedCount [expr {$allowedCount + ($maxIP - $current + 1)}]
    }
    return $allowedCount
}

# Main program
set filename "input.txt"
set ranges [readRanges $filename]

# Part 1: Find the lowest unblocked IP
set lowestUnblockedIP [findLowestUnblockedIP $ranges]
puts "Lowest unblocked IP: $lowestUnblockedIP"

# Part 2: Count the number of allowed IPs
set allowedIPs [countAllowedIPs $ranges]
puts "Number of allowed IPs: $allowedIPs"
