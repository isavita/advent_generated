set file [open "input.txt" r]
set caloriesList {}
set currentCalories 0

while {[gets $file line] >= 0} {
    if {$line eq ""} {
        lappend caloriesList $currentCalories
        set currentCalories 0
    } else {
        set currentCalories [expr {$currentCalories + $line}]
    }
}
lappend caloriesList $currentCalories
close $file

set sortedCalories [lsort -decreasing -integer $caloriesList]
set topThreeSum 0

for {set i 0} {$i < 3 && $i < [llength $sortedCalories]} {incr i} {
    set topThreeSum [expr {$topThreeSum + [lindex $sortedCalories $i]}]
}

puts $topThreeSum