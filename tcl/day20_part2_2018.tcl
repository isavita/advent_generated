
proc main {} {
    set inputFile "input.txt"
    set fh [open $inputFile r]
    set directions [read $fh]
    close $fh
    set directions [string range $directions 1 [expr {[string length $directions] - 2}]]

    array set rooms {}
    array set doors_passed {}

    set current_room "0,0"
    set doors_passed($current_room) 0
    set rooms($current_room) 0

    set stack [list]

    array set moves {
        N "0,-1"
        E "1,0"
        S "0,1"
        W "-1,0"
    }

    foreach char [split $directions ""] {
        if {$char eq "("} {
            lappend stack [list $current_room [array get doors_passed]]
        } elseif {$char eq "|"} {
            set state [lindex $stack end]
            set current_room [lindex $state 0]
            array unset doors_passed
            array set doors_passed [lindex $state 1]
        } elseif {$char eq ")"} {
            set state [lindex $stack end]
            set stack [lreplace $stack end end]
            set current_room [lindex $state 0]
            array unset doors_passed
            array set doors_passed [lindex $state 1]
        } elseif {[info exists moves($char)]} {
            set delta [split $moves($char) ","]
            set dx [lindex $delta 0]
            set dy [lindex $delta 1]

            set current_coords [split $current_room ","]
            set cx [lindex $current_coords 0]
            set cy [lindex $current_coords 1]

            set nx [expr {$cx + $dx}]
            set ny [expr {$cy + $dy}]
            set new_room "$nx,$ny"

            set new_doors [expr {$doors_passed($current_room) + 1}]

            set doors_passed($new_room) $new_doors

            if {![info exists rooms($new_room)] || $new_doors < $rooms($new_room)} {
                set rooms($new_room) $new_doors
            }

            set current_room $new_room
        }
    }

    set max_doors 0
    set rooms_with_1000_doors 0
    foreach key [array names rooms] {
        set doors $rooms($key)
        if {$doors > $max_doors} {
            set max_doors $doors
        }
        if {$doors >= 1000} {
            incr rooms_with_1000_doors
        }
    }

    puts $max_doors
    puts $rooms_with_1000_doors
}

main
