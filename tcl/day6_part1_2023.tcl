
#!/usr/bin/env tclsh

proc calculateWaysToWin {time record} {
    set waysToWin 0
    for {set holdTime 1} {$holdTime < $time} {incr holdTime} {
        set travelTime [expr {$time - $holdTime}]
        set distance [expr {$holdTime * $travelTime}]
        if {$distance > $record} {
            incr waysToWin
        }
    }
    return $waysToWin
}

set file [open "input.txt" r]
set content [read $file]
close $file

set lines [split $content "\n"]
set times [regexp -all -inline {\d+} [lindex $lines 0]]
set distances [regexp -all -inline {\d+} [lindex $lines 1]]

set totalWays 1
for {set i 0} {$i < [llength $times]} {incr i} {
    set ways [calculateWaysToWin [lindex $times $i] [lindex $distances $i]]
    set totalWays [expr {$totalWays * $ways}]
}

puts $totalWays
