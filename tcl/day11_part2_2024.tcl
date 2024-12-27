
proc trimLeadingZeros s {
    set i 0
    while {$i < [string length $s] - 1 && [string index $s $i] eq "0"} {
        incr i
    }
    return [string range $s $i end]
}

proc splitStone s {
    set mid [expr {[string length $s] / 2}]
    set left [trimLeadingZeros [string range $s 0 [expr {$mid - 1}]]]
    set right [trimLeadingZeros [string range $s $mid end]]
    if {$left eq ""} {
        set left "0"
    }
    if {$right eq ""} {
        set right "0"
    }
    return [list $left $right]
}

proc multiplyBy2024 s {
    set num [split $s ""]
    set multiplier [split "2024" ""]
    set result [lrepeat [expr {[llength $num] + [llength $multiplier]}] 0]
    for {set i [expr {[llength $num] - 1}]} {$i >= 0} {incr i -1} {
        set carry 0
        for {set j [expr {[llength $multiplier] - 1}]} {$j >= 0} {incr j -1} {
            set product [expr {[lindex $num $i] * [lindex $multiplier $j] + [lindex $result [expr {$i + $j + 1}]] + $carry}]
            lset result [expr {$i + $j + 1}] [expr {$product % 10}]
            set carry [expr {$product / 10}]
        }
        lset result $i [expr {[lindex $result $i] + $carry}]
    }
    set start 0
    while {$start < [llength $result] - 1 && [lindex $result $start] == 0} {
        incr start
    }
    set sb ""
    for {set i $start} {$i < [llength $result]} {incr i} {
        append sb [lindex $result $i]
    }
    return $sb
}

set file [open "input.txt" r]
if {[eof $file]} {
    puts "Input file is empty"
    close $file
    return
}
set line [read $file]
close $file
set stonesStr [split $line]

set stonesMap [dict create]
foreach s $stonesStr {
    if {[dict exists $stonesMap $s]} {
        dict incr stonesMap $s
    } else {
        dict set stonesMap $s 1
    }
}

for {set step 0} {$step < 75} {incr step} {
    set newStonesMap [dict create]
    dict for {stone count} $stonesMap {
        if {$stone eq "0"} {
            dict incr newStonesMap "1" $count
        } elseif {[expr {[string length $stone] % 2}] == 0} {
            lassign [splitStone $stone] left right
            dict incr newStonesMap $left $count
            dict incr newStonesMap $right $count
        } else {
            set newStone [multiplyBy2024 $stone]
            dict incr newStonesMap $newStone $count
        }
    }
    set stonesMap $newStonesMap
}

set totalStones 0
dict for {stone count} $stonesMap {
    incr totalStones $count
}

puts $totalStones
