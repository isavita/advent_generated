
#!/usr/bin/env tclsh

proc minMax {a b} {
    if {$a < $b} {
        return [list $a $b]
    }
    return [list $b $a]
}

proc giveChip {botsVar target value} {
    upvar $botsVar bots
    if {![dict exists $bots $target]} {
        dict set bots $target [dict create chips [list]]
    }
    set chips [dict get $bots $target chips]
    lappend chips $value
    dict set bots $target chips $chips
}

proc main {} {
    set file [open "input.txt" r]
    set content [read $file]
    close $file

    set bots [dict create]
    set valueRegex {value (\d+) goes to (bot \d+)}
    set givesRegex {(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)}

    foreach line [split $content "\n"] {
        if {[regexp $valueRegex $line -> value botID]} {
            if {![dict exists $bots $botID]} {
                dict set bots $botID [dict create chips [list] lowTo {} highTo {}]
            }
            set chips [dict get $bots $botID chips]
            lappend chips $value
            dict set bots $botID chips $chips
        } elseif {[regexp $givesRegex $line -> botID lowTo highTo]} {
            if {![dict exists $bots $botID]} {
                dict set bots $botID [dict create chips [list] lowTo {} highTo {}]
            }
            dict set bots $botID lowTo $lowTo
            dict set bots $botID highTo $highTo
        }
    }

    while {1} {
        set action 0
        dict for {botID bot} $bots {
            set chips [dict get $bot chips]
            if {[llength $chips] == 2} {
                set action 1
                lassign [minMax [lindex $chips 0] [lindex $chips 1]] low high

                if {$low == 17 && $high == 61} {
                    puts $botID
                    return
                }

                dict set bots $botID chips [list]
                giveChip bots [dict get $bot lowTo] $low
                giveChip bots [dict get $bot highTo] $high
            }
        }
        if {!$action} break
    }
}

main
