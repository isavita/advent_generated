set file [open "input.txt" r]
set instructions [split [read $file] "\n"]
close $file

proc producesClockSignal {a instructions} {
    set registers [dict create a $a b 0 c 0 d 0]
    set lastOutput 0
    set outputCount 0
    set i 0

    while {$i < [llength $instructions]} {
        set parts [split [lindex $instructions $i] " "]
        switch [lindex $parts 0] {
            "cpy" {
                set val [getValue [lindex $parts 1] $registers]
                dict set registers [lindex $parts 2] $val
            }
            "inc" {
                dict incr registers [lindex $parts 1]
            }
            "dec" {
                dict incr registers [lindex $parts 1] -1
            }
            "jnz" {
                set val [getValue [lindex $parts 1] $registers]
                if {$val != 0} {
                    set jump [getValue [lindex $parts 2] $registers]
                    set i [expr {$i + $jump}]
                    continue
                }
            }
            "out" {
                set val [getValue [lindex $parts 1] $registers]
                if {$val != 0 && $val != 1} { return 0 }
                if {$outputCount > 0 && $val == $lastOutput} { return 0 }
                set lastOutput $val
                incr outputCount
                if {$outputCount > 50} { return 1 }
            }
        }
        incr i
    }
    return 0
}

proc getValue {s registers} {
    if {[string is integer $s]} {
        return $s
    }
    return [dict get $registers $s]
}

set found 0
for {set a 1} {$found == 0} {incr a} {
    if {[producesClockSignal $a $instructions]} {
        puts $a
        set found 1
    }
}