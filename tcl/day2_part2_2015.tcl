set file [open "input.txt" r]
set totalRibbon 0

while {[gets $file line] >= 0} {
    set dimensions [split $line "x"]
    if {[llength $dimensions] != 3} {
        puts "Invalid input format"
        continue
    }

    set l [expr {[lindex $dimensions 0]}]
    set w [expr {[lindex $dimensions 1]}]
    set h [expr {[lindex $dimensions 2]}]

    set bow [expr {$l * $w * $h}]
    set sides [list $l $w $h]
    set sortedSides [lsort -integer $sides]
    set wrap [expr {2 * [lindex $sortedSides 0] + 2 * [lindex $sortedSides 1]}]

    set totalRibbon [expr {$totalRibbon + $bow + $wrap}]
}

close $file
puts $totalRibbon