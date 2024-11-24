
# Function to read and parse the input file
proc readBlockedRanges {filename} {
    set blockedRanges {}
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        lassign [split $line "-"] start end
        lappend blockedRanges [list $start $end]
    }
    close $file
    return $blockedRanges
}

# Function to find the lowest unblocked IP
proc findLowestUnblockedIP {blockedRanges} {
    set blockedRanges [lsort -integer -index 0 $blockedRanges]
    set currentIP 0

    foreach range $blockedRanges {
        lassign $range start end
        if {$currentIP < $start} {
            return $currentIP
        }
        if {$currentIP <= $end} {
            set currentIP [expr {$end + 1}]
        }
    }
    return $currentIP
}

# Main program
set filename "input.txt"
set blockedRanges [readBlockedRanges $filename]
set lowestUnblockedIP [findLowestUnblockedIP $blockedRanges]
puts "The lowest unblocked IP is: $lowestUnblockedIP"
