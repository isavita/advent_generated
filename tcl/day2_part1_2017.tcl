set infile [open "input.txt" r]
set data [read $infile]
close $infile

set lines [split $data "\n"]
set checksum 0

foreach line $lines {
    set nums [split $line]
    set minVal [lindex $nums 0]
    set maxVal [lindex $nums 0]

    foreach num $nums {
        if {$num < $minVal} {set minVal $num}
        if {$num > $maxVal} {set maxVal $num}
    }

    set checksum [expr {$checksum + ($maxVal - $minVal)}]
}

puts $checksum