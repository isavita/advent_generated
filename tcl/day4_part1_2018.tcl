
#!/usr/bin/env tclsh

proc parseRecords {filename} {
    set records {}
    set file [open $filename r]
    while {[gets $file line] != -1} {
        if {[regexp {\[(.+)\] (.+)} $line -> timestamp action]} {
            set record [dict create timestamp $timestamp action $action guardId -1]
            
            if {[regexp {Guard #(\d+)} $action -> guardId]} {
                dict set record action "begins shift"
                dict set record guardId $guardId
            } elseif {[string first "falls asleep" $action] != -1} {
                dict set record action "falls asleep"
            } elseif {[string first "wakes up" $action] != -1} {
                dict set record action "wakes up"
            }
            
            lappend records $record
        }
    }
    close $file
    
    return [lsort -dictionary -index 1 $records]
}

proc analyzeGuardSleep {records} {
    array set guardSleepMinutes {}
    set currentGuardId -1
    set sleepStart 0

    foreach record $records {
        set action [dict get $record action]
        
        switch $action {
            "begins shift" {
                set currentGuardId [dict get $record guardId]
            }
            "falls asleep" {
                set sleepStart [scan [lindex [split [dict get $record timestamp] :] 1] %d]
            }
            "wakes up" {
                set wakeTime [scan [lindex [split [dict get $record timestamp] :] 1] %d]
                for {set i $sleepStart} {$i < $wakeTime} {incr i} {
                    if {![info exists guardSleepMinutes($currentGuardId)]} {
                        set guardSleepMinutes($currentGuardId) [lrepeat 60 0]
                    }
                    lset guardSleepMinutes($currentGuardId) $i [expr {[lindex $guardSleepMinutes($currentGuardId) $i] + 1}]
                }
            }
        }
    }

    set maxSleep 0
    set sleepiestGuard -1

    foreach {guardId minutes} [array get guardSleepMinutes] {
        set totalSleep [::tcl::mathop::+ {*}$minutes]
        if {$totalSleep > $maxSleep} {
            set maxSleep $totalSleep
            set sleepiestGuard $guardId
        }
    }

    set maxMinute 0
    set maxMinuteCount 0
    set sleepiestMinutes $guardSleepMinutes($sleepiestGuard)

    for {set i 0} {$i < 60} {incr i} {
        set count [lindex $sleepiestMinutes $i]
        if {$count > $maxMinuteCount} {
            set maxMinuteCount $count
            set maxMinute $i
        }
    }

    return [expr {$sleepiestGuard * $maxMinute}]
}

set records [parseRecords "input.txt"]
puts [analyzeGuardSleep $records]
