proc countUniqueSegments {filename} {
    set uniqueCount 0
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        set parts [split $line "|"]
        set outputValues [lindex $parts 1]
        foreach value [split $outputValues] {
            set length [string length $value]
            if {$length == 2 || $length == 3 || $length == 4 || $length == 7} {
                incr uniqueCount
            }
        }
    }
    close $file
    return $uniqueCount
}

puts [countUniqueSegments "input.txt"]