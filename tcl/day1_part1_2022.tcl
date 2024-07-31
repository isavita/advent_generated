set maxCalories 0
set currentCalories 0

set file [open "input.txt" r]
while {[gets $file line] >= 0} {
    if {$line eq ""} {
        if {$currentCalories > $maxCalories} {
            set maxCalories $currentCalories
        }
        set currentCalories 0
    } else {
        set calories [expr {$line}]
        set currentCalories [expr {$currentCalories + $calories}]
    }
}
close $file

if {$currentCalories > $maxCalories} {
    set maxCalories $currentCalories
}

puts $maxCalories