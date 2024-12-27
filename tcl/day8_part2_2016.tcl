
proc rect {screen a b} {
    for {set y 0} {$y < $b} {incr y} {
        for {set x 0} {$x < $a} {incr x} {
            lset screen $y $x 1
        }
    }
    return $screen
}

proc rotateRow {screen row shift} {
    set temp {}
    foreach pixel [lindex $screen $row] {
        lappend temp $pixel
    }
    set width [llength [lindex $screen 0]]
    set newRow {}
    for {set i 0} {$i < $width} {incr i} {
        lappend newRow [lindex $temp [expr {($i - $shift + $width) % $width}]]
    }
    lset screen $row $newRow
    return $screen
}

proc rotateColumn {screen col shift} {
    set height [llength $screen]
    set temp {}
    for {set i 0} {$i < $height} {incr i} {
        lappend temp [lindex [lindex $screen $i] $col]
    }
    for {set i 0} {$i < $height} {incr i} {
        lset screen $i $col [lindex $temp [expr {($i - $shift + $height) % $height}]]
    }
    return $screen
}

proc processInstruction {instruction screen} {
    if {[regexp {^rect (\d+)x(\d+)$} $instruction -> a b]} {
        return [rect $screen $a $b]
    } elseif {[regexp {^rotate row y=(\d+) by (\d+)$} $instruction -> row shift]} {
        return [rotateRow $screen $row $shift]
    } elseif {[regexp {^rotate column x=(\d+) by (\d+)$} $instruction -> col shift]} {
        return [rotateColumn $screen $col $shift]
    }
    return $screen
}

proc displayScreen {screen} {
    foreach row $screen {
        foreach pixel $row {
            if {$pixel} {
                puts -nonewline "#"
            } else {
                puts -nonewline "."
            }
        }
        puts ""
    }
}

proc countLitPixels {screen} {
    set count 0
    foreach row $screen {
        foreach pixel $row {
            if {$pixel} {
                incr count
            }
        }
    }
    return $count
}

set screenHeight 6
set screenWidth 50
set screen {}
for {set i 0} {$i < $screenHeight} {incr i} {
    lappend screen [lrepeat $screenWidth 0]
}

set file [open "input.txt" r]
while {[gets $file line] != -1} {
    set screen [processInstruction $line $screen]
}
close $file

displayScreen $screen
puts ""
puts [countLitPixels $screen]
