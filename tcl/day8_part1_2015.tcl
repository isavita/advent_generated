set file [open "input.txt" r]
set totalDiff 0

proc calculateMemoryLength {s} {
    set length 0
    set inEscape 0
    set hexCount 0

    set s [string range $s 1 end-1]
    foreach char [split $s ""] {
        if {$hexCount > 0} {
            incr hexCount -1
        } elseif {$inEscape} {
            if {$char eq "x"} {
                set hexCount 2
            }
            set inEscape 0
            incr length
        } elseif {$char eq "\\"} {
            set inEscape 1
        } else {
            incr length
        }
    }
    return $length
}

while {[gets $file line] >= 0} {
    set codeLength [string length $line]
    set memoryLength [calculateMemoryLength $line]
    set totalDiff [expr {$totalDiff + $codeLength - $memoryLength}]
}

close $file
puts $totalDiff