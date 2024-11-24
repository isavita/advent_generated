
#!/usr/bin/env tclsh

proc parseMirror {mirrorStr} {
    set rows {}
    set cols {}
    set width [string length [lindex $mirrorStr 0]]
    
    # Initialize column bit representations
    for {set x 0} {$x < $width} {incr x} {
        lappend cols 0
    }
    
    # Process each row
    foreach line $mirrorStr {
        set rowBit 0
        for {set x 0} {$x < $width} {incr x} {
            set rowBit [expr {$rowBit << 1}]
            set colBit [lindex $cols $x]
            set colBit [expr {$colBit << 1}]
            
            if {[string index $line $x] eq "#"} {
                incr rowBit
                lset cols $x [expr {$colBit + 1}]
            } else {
                lset cols $x $colBit
            }
        }
        lappend rows $rowBit
    }
    
    return [list $rows $cols]
}

proc getMirrorAxisWithOneSmudge {lines} {
    set len [llength $lines]
    
    for {set i 1} {$i < $len} {incr i} {
        set isMirror 1
        set numSmudges 0
        
        for {set j 0} {$isMirror && $j < min($i, $len-$i)} {incr j} {
            set line1 [lindex $lines [expr {$i-1-$j}]]
            set line2 [lindex $lines [expr {$i+$j}]]
            
            if {$line1 != $line2} {
                if {$numSmudges > 0} {
                    set isMirror 0
                } else {
                    set dif [expr {$line1 ^ $line2}]
                    set isOnlyOneSmudge [expr {($dif & ($dif - 1)) == 0}]
                    
                    if {$isOnlyOneSmudge} {
                        incr numSmudges
                    } else {
                        set isMirror 0
                    }
                }
            }
        }
        
        if {$isMirror && $numSmudges == 1} {
            return $i
        }
    }
    
    return 0
}

proc solve {input} {
    set mirrors {}
    set currentMirror {}
    
    foreach line $input {
        if {$line eq ""} {
            lappend mirrors [parseMirror $currentMirror]
            set currentMirror {}
        } else {
            lappend currentMirror $line
        }
    }
    
    if {[llength $currentMirror] > 0} {
        lappend mirrors [parseMirror $currentMirror]
    }
    
    set res 0
    foreach mirror $mirrors {
        lassign $mirror rows cols
        
        set colAxis [getMirrorAxisWithOneSmudge $cols]
        set rowAxis [getMirrorAxisWithOneSmudge $rows]
        
        incr res $colAxis
        incr res [expr {$rowAxis * 100}]
    }
    
    return $res
}

proc min {a b} {
    return [expr {$a < $b ? $a : $b}]
}

# Read input from file
set fp [open "input.txt" r]
set input [split [read -nonewline $fp] "\n"]
close $fp

# Solve and print result
puts [solve $input]
