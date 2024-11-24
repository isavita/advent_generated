
set fp [open "input.txt" r]
if {$fp == -1} {
    puts "Error opening file"
    exit 1
}

set line [gets $fp]
regsub -all {\s+} $line "" line
set time [lindex [split $line ":"] 1]

set line [gets $fp]
regsub -all {\s+} $line "" line
set distance [lindex [split $line ":"] 1]

close $fp

set waysToWin 0
for {set holdTime 1} {$holdTime < $time} {incr holdTime} {
    set travelTime [expr {$time - $holdTime}]
    set distanceTraveled [expr {$holdTime * $travelTime}]
    if {$distanceTraveled > $distance} {
        incr waysToWin
    }
}

puts $waysToWin
