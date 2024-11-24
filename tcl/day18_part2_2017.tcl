
#!/usr/bin/env tclsh

proc getValue {arg registersVar} {
    upvar $registersVar registers
    if {[string is integer -strict $arg]} {
        return $arg
    }
    return [dict get $registers $arg]
}

proc main {} {
    set file [open "input.txt" r]
    set instructions [list]
    
    while {[gets $file line] != -1} {
        lappend instructions [split $line]
    }
    close $file

    set registers0 [dict create p 0]
    set registers1 [dict create p 1]
    set queue0 [list]
    set queue1 [list]
    set sendCount1 0
    set i0 0
    set i1 0
    set deadlock0 0
    set deadlock1 0

    while {!($deadlock0 && $deadlock1)} {
        set deadlock0 1
        set deadlock1 1

        # Program 0
        while {$i0 < [llength $instructions]} {
            set instruction [lindex $instructions $i0]
            set cmd [lindex $instruction 0]
            set arg1 [lindex $instruction 1]

            switch $cmd {
                "snd" {
                    lappend queue1 [getValue $arg1 registers0]
                }
                "set" {
                    dict set registers0 $arg1 [getValue [lindex $instruction 2] registers0]
                }
                "add" {
                    dict set registers0 $arg1 [expr {[dict get $registers0 $arg1] + [getValue [lindex $instruction 2] registers0]}]
                }
                "mul" {
                    dict set registers0 $arg1 [expr {[dict get $registers0 $arg1] * [getValue [lindex $instruction 2] registers0]}]
                }
                "mod" {
                    dict set registers0 $arg1 [expr {[dict get $registers0 $arg1] % [getValue [lindex $instruction 2] registers0]}]
                }
                "rcv" {
                    if {[llength $queue0] == 0} {
                        break
                    }
                    dict set registers0 $arg1 [lindex $queue0 0]
                    set queue0 [lrange $queue0 1 end]
                }
                "jgz" {
                    if {[getValue $arg1 registers0] > 0} {
                        incr i0 [getValue [lindex $instruction 2] registers0]
                        continue
                    }
                }
            }
            incr i0
            set deadlock0 0
        }

        # Program 1
        while {$i1 < [llength $instructions]} {
            set instruction [lindex $instructions $i1]
            set cmd [lindex $instruction 0]
            set arg1 [lindex $instruction 1]

            switch $cmd {
                "snd" {
                    lappend queue0 [getValue $arg1 registers1]
                    incr sendCount1
                }
                "set" {
                    dict set registers1 $arg1 [getValue [lindex $instruction 2] registers1]
                }
                "add" {
                    dict set registers1 $arg1 [expr {[dict get $registers1 $arg1] + [getValue [lindex $instruction 2] registers1]}]
                }
                "mul" {
                    dict set registers1 $arg1 [expr {[dict get $registers1 $arg1] * [getValue [lindex $instruction 2] registers1]}]
                }
                "mod" {
                    dict set registers1 $arg1 [expr {[dict get $registers1 $arg1] % [getValue [lindex $instruction 2] registers1]}]
                }
                "rcv" {
                    if {[llength $queue1] == 0} {
                        break
                    }
                    dict set registers1 $arg1 [lindex $queue1 0]
                    set queue1 [lrange $queue1 1 end]
                }
                "jgz" {
                    if {[getValue $arg1 registers1] > 0} {
                        incr i1 [getValue [lindex $instruction 2] registers1]
                        continue
                    }
                }
            }
            incr i1
            set deadlock1 0
        }
    }

    puts $sendCount1
}

main
