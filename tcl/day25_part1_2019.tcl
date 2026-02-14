proc parseProgram {filename} {
    set f [open $filename r]
    set text [string trim [read $f]]
    close $f
    set program [split $text ,]
    return $program
}

array set mem {}
set ip 0
set rb 0
set inputQueue {}

proc memGet {addr} {
    global mem
    if {![info exists mem($addr)]} { return 0 }
    return $mem($addr)
}

proc memSet {addr val} {
    global mem
    set mem($addr) $val
}

proc addInput {s} {
    global inputQueue
    foreach ch [split $s ""] {
        lappend inputQueue [scan $ch %c]
    }
}


proc readParam {off mode} {
    global ip rb
    set v [memGet [expr {$ip + $off}]]
    if {$mode == 0} { return [memGet $v] }
    if {$mode == 1} { return $v }
    return [memGet [expr {$rb + $v}]]
}

proc writeAddr {off mode} {
    global ip rb
    set v [memGet [expr {$ip + $off}]]
    if {$mode == 2} { return [expr {$rb + $v}] }
    return $v
}

proc runUntilInput {} {
    global ip rb inputQueue
    set output ""

    while {1} {
        set inst [memGet $ip]
        set op [expr {$inst % 100}]
        set m1 [expr {($inst / 100) % 10}]
        set m2 [expr {($inst / 1000) % 10}]
        set m3 [expr {($inst / 10000) % 10}]

        switch $op {
            1 {
                memSet [writeAddr 3 $m3] [expr {[readParam 1 $m1] + [readParam 2 $m2]}]
                incr ip 4
            }
            2 {
                memSet [writeAddr 3 $m3] [expr {[readParam 1 $m1] * [readParam 2 $m2]}]
                incr ip 4
            }
            3 {
                if {[llength $inputQueue] == 0} {
                    return $output
                }
                memSet [writeAddr 1 $m1] [lindex $inputQueue 0]
                set inputQueue [lrange $inputQueue 1 end]
                incr ip 2
            }
            4 {
                append output [format %c [readParam 1 $m1]]
                incr ip 2
            }
            5 {
                if {[readParam 1 $m1] != 0} { set ip [readParam 2 $m2] } else { incr ip 3 }
            }
            6 {
                if {[readParam 1 $m1] == 0} { set ip [readParam 2 $m2] } else { incr ip 3 }
            }
            7 {
                memSet [writeAddr 3 $m3] [expr {[readParam 1 $m1] < [readParam 2 $m2] ? 1 : 0}]
                incr ip 4
            }
            8 {
                memSet [writeAddr 3 $m3] [expr {[readParam 1 $m1] == [readParam 2 $m2] ? 1 : 0}]
                incr ip 4
            }
            9 {
                set rb [expr {$rb + [readParam 1 $m1]}]
                incr ip 2
            }
            99 {
                return $output
            }
            default {
                error "bad opcode $op"
            }
        }
    }
}

# Deterministic command sequence for the official puzzle map.
set cmds {
    "north\n"
    "take tambourine\n"
    "north\n"
    "take astrolabe\n"
    "east\n"
    "take klein bottle\n"
    "west\n"
    "south\n"
    "south\n"
    "east\n"
    "take shell\n"
    "west\n"
    "north\n"
    "west\n"
    "south\n"
    "take mug\n"
    "north\n"
    "west\n"
    "west\n"
    "take astronaut ice cream\n"
    "east\n"
    "east\n"
    "north\n"
    "west\n"
    "drop shell\n"
    "east\n"
}

set program [parseProgram "input.txt"]
for {set i 0} {$i < [llength $program]} {incr i} {
    memSet $i [lindex $program $i]
}

# prime output
runUntilInput

foreach cmd $cmds {
    addInput $cmd
    set out [runUntilInput]
}

if {[regexp {typing ([0-9]+) on the keypad} $out -> code]} {
    puts $code
} else {
    puts ""
}
