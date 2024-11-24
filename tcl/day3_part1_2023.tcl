
#!/usr/bin/env tclsh

proc readFileToMatrix {filePath} {
    set matrix {}
    set file [open $filePath r]
    while {[gets $file line] != -1} {
        lappend matrix [split $line ""]
    }
    close $file
    return $matrix
}

proc isDigit {char} {
    return [regexp {^\d$} $char]
}

proc isSymbol {char} {
    return [expr {$char ne "." && ![isDigit $char]}]
}

proc extractNumber {matrix x y} {
    set numberStr ""
    set length 0
    set row [lindex $matrix $y]
    while {$x < [llength $row] && [isDigit [lindex $row $x]]} {
        append numberStr [lindex $row $x]
        incr x
        incr length
    }
    return [list [scan $numberStr %d] $length]
}

proc checkAdjacent {matrix x y} {
    set height [llength $matrix]
    set width [llength [lindex $matrix 0]]
    
    for {set dy -1} {$dy <= 1} {incr dy} {
        for {set dx -1} {$dx <= 1} {incr dx} {
            set adjX [expr {$x + $dx}]
            set adjY [expr {$y + $dy}]
            
            if {$adjY >= 0 && $adjY < $height && 
                $adjX >= 0 && $adjX < [llength [lindex $matrix $adjY]]} {
                set adjChar [lindex [lindex $matrix $adjY] $adjX]
                if {[isSymbol $adjChar]} {
                    return 1
                }
            }
        }
    }
    return 0
}

proc sumOfPartNumbers {matrix} {
    set sum 0
    set height [llength $matrix]
    set width [llength [lindex $matrix 0]]
    
    set visited {}
    for {set y 0} {$y < $height} {incr y} {
        set rowVisited {}
        for {set x 0} {$x < $width} {incr x} {
            lappend rowVisited 0
        }
        lappend visited $rowVisited
    }
    
    for {set y 0} {$y < $height} {incr y} {
        for {set x 0} {$x < $width} {incr x} {
            if {![lindex [lindex $visited $y] $x] && 
                [isDigit [lindex [lindex $matrix $y] $x]]} {
                
                lassign [extractNumber $matrix $x $y] number length
                
                set isPartNumber 0
                for {set i 0} {$i < $length} {incr i} {
                    if {[checkAdjacent $matrix [expr {$x + $i}] $y]} {
                        set isPartNumber 1
                        break
                    }
                }
                
                if {$isPartNumber} {
                    incr sum $number
                }
                
                for {set i 0} {$i < $length} {incr i} {
                    lset visited $y [expr {$x + $i}] 1
                }
                
                set x [expr {$x + $length - 1}]
            }
        }
    }
    
    return $sum
}

set matrix [readFileToMatrix "input.txt"]
puts [sumOfPartNumbers $matrix]
