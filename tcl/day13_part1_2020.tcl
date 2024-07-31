set file [open "input.txt" r]
set earliestDeparture [gets $file]
set busIDs [split [gets $file] ","]

set earliestBusID 0
set minWaitTime $earliestDeparture

foreach id $busIDs {
    if {$id ne "x"} {
        set busID [expr {$id}]
        set waitTime [expr {$busID - ($earliestDeparture % $busID)}]
        if {$waitTime < $minWaitTime} {
            set minWaitTime $waitTime
            set earliestBusID $busID
        }
    }
}

puts [expr {$earliestBusID * $minWaitTime}]
close $file