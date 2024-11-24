
#!/usr/bin/env tclsh

proc convertNumber {number ranges} {
    foreach range $ranges {
        lassign $range destStart srcStart length
        if {$number >= $srcStart && $number < $srcStart + $length} {
            return [expr {$destStart + ($number - $srcStart)}]
        }
    }
    return $number
}

# Read input file
set fp [open "input.txt" r]
set file_data [read $fp]
close $fp

# Parse input
set lines [split $file_data "\n"]
set seeds {}
set maps {}
set currentRanges {}

foreach line $lines {
    if {[string first "map:" $line] != -1} {
        if {[llength $currentRanges] > 0} {
            lappend maps $currentRanges
            set currentRanges {}
        }
    } elseif {[string first "seeds:" $line] != -1} {
        set seeds [lmap seed [lrange [split $line] 1 end] {expr {int($seed)}}]
    } elseif {[llength [split $line]] == 3} {
        lassign [split $line] destStart srcStart length
        lappend currentRanges [list [expr {int($destStart)}] [expr {int($srcStart)}] [expr {int($length)}]]
    }
}

# Add last map
lappend maps $currentRanges

# Find minimum location
set minLocation -1
foreach seed $seeds {
    set location $seed
    foreach m $maps {
        set location [convertNumber $location $m]
    }

    if {$minLocation == -1 || $location < $minLocation} {
        set minLocation $location
    }
}

puts $minLocation
