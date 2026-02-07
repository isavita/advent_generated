#!/usr/bin/env tclsh

set f [open "input.txt" r]
set grid {}
while {[gets $f line] >= 0} {
    if {$line ne ""} {lappend grid $line}
}
close $f

set height [llength $grid]
if {$height == 0} {
    puts 0
    exit
}
set width [string length [lindex $grid 0]]

set startX -1
set startY -1
for {set y 0} {$y < $height && $startX == -1} {incr y} {
    set row [lindex $grid $y]
    for {set x 0} {$x < $width} {incr x} {
        if {[string index $row $x] eq "S"} {
            set startX $x
            set startY $y
            break
        }
    }
}
if {$startX == -1} {
    puts stderr "Start point 'S' not found"
    exit 1
}

set counts [dict create]
dict set counts $startX 1

for {set y $startY} {$y < $height} {incr y} {
    set row [lindex $grid $y]
    set nextCounts [dict create]
    dict for {x cnt} $counts {
        set isSplitter 0
        if {$x >= 0 && $x < $width && [string index $row $x] eq "^"} {
            set isSplitter 1
        }
        if {$isSplitter} {
            foreach nx [list [expr {$x-1}] [expr {$x+1}]] {
                dict incr nextCounts $nx $cnt
            }
        } else {
            dict incr nextCounts $x $cnt
        }
    }
    set counts $nextCounts
}

set total 0
dict for {x cnt} $counts {
    set total [expr {$total + $cnt}]
}
puts $total