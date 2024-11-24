
#!/usr/bin/env tclsh

proc swapPositions {pwVar x y} {
    upvar $pwVar pw
    set temp [lindex $pw $x]
    lset pw $x [lindex $pw $y]
    lset pw $y $temp
}

proc swapLetters {pwVar x y} {
    upvar $pwVar pw
    set xIndex [lsearch $pw $x]
    set yIndex [lsearch $pw $y]
    swapPositions pw $xIndex $yIndex
}

proc rotate {pwVar steps} {
    upvar $pwVar pw
    set len [llength $pw]
    set steps [expr {$steps % $len}]
    if {$steps < 0} {
        set steps [expr {$steps + $len}]
    }
    set pw [concat [lrange $pw [expr {$len - $steps}] end] [lrange $pw 0 [expr {$len - $steps - 1}]]]
}

proc rotateLetter {pwVar x} {
    upvar $pwVar pw
    set index [lsearch $pw $x]
    if {$index >= 4} {
        incr index
    }
    rotate pw [expr {$index + 1}]
}

proc derotateLetter {pwVar x} {
    upvar $pwVar pw
    set index [lsearch $pw $x]
    if {$index % 2 == 1} {
        set rot [expr {-($index + 1) / 2}]
    } elseif {$index != 0} {
        set rot [expr {(6 - $index) / 2}]
    } else {
        set rot -1
    }
    rotate pw $rot
}

proc reverseRange {pwVar x y} {
    upvar $pwVar pw
    while {$x < $y} {
        swapPositions pw $x $y
        incr x
        incr y -1
    }
}

proc move {pwVar x y} {
    upvar $pwVar pw
    set ch [lindex $pw $x]
    set pw [lreplace $pw $x $x]
    set pw [linsert $pw $y $ch]
}

proc scramble {pwVar instructions {direction 1}} {
    upvar $pwVar pw
    
    if {$direction < 0} {
        set instructions [lreverse $instructions]
    }
    
    foreach instruction $instructions {
        set line [split $instruction]
        
        switch -glob $instruction {
            "swap position*" {
                set x [lindex $line 2]
                set y [lindex $line end]
                swapPositions pw $x $y
            }
            "swap letter*" {
                set x [lindex $line 2]
                set y [lindex $line end]
                swapLetters pw $x $y
            }
            "rotate left*" {
                set x [lindex $line 2]
                set x [expr {-$x}]
                if {$direction < 0} {
                    set x [expr {-$x}]
                }
                rotate pw $x
            }
            "rotate right*" {
                set x [lindex $line 2]
                if {$direction < 0} {
                    set x [expr {-$x}]
                }
                rotate pw $x
            }
            "rotate based*" {
                set x [string index [lindex $line end] 0]
                if {$direction > 0} {
                    rotateLetter pw $x
                } else {
                    derotateLetter pw $x
                }
            }
            "reverse*" {
                set x [lindex $line 2]
                set y [lindex $line end]
                reverseRange pw $x $y
            }
            "move*" {
                set x [lindex $line 2]
                set y [lindex $line end]
                if {$direction < 0} {
                    set tmp $x
                    set x $y
                    set y $tmp
                }
                move pw $x $y
            }
        }
    }
    return $pw
}

proc unscramble {pwVar instructions} {
    upvar $pwVar pw
    return [scramble pw $instructions -1]
}

# Main program
set fp [open "input.txt" r]
set instructions [split [read $fp] "\n"]
close $fp

set hashed [split "fbgdceah" ""]
set result [unscramble hashed $instructions]
puts [join $result ""]
