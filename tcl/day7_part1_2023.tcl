
#!/usr/bin/tclsh

proc classify_hand {cards} {
    set counts [dict create]
    foreach char [split $cards ""] {
        dict incr counts $char
    }
    set value 1
    set size [dict size $counts]
    foreach count [dict values $counts] {
        set value [expr {$value * $count}]
    }
    if {$value == 1} {return 0}
    if {$value == 2} {return 1}
    if {$value == 3} {return 3}
    if {$value == 5} {return 6}
    if {$value == 6} {return 4}
    if {$value == 4} {
        if {$size == 2} {return 5}
        return 2
    }
    return -1
}

proc main {} {
    set hands [list]
    set fileId [open "input.txt" r]
    set content [read $fileId]
    close $fileId
    set lines [split $content "\n"]
    foreach line $lines {
        if {$line eq ""} continue
        set parts [split $line " "]
        set cards [lindex $parts 0]
        set bid [lindex $parts 1]
        lappend hands [list $cards $bid]
    }
    set processed_hands [list]
    set map_rules {A E K D Q C J B T A}
    foreach hand_pair $hands {
        set cards [lindex $hand_pair 0]
        set bid [lindex $hand_pair 1]
        set category [classify_hand $cards]
        set mapped_cards [string map $map_rules $cards]
        lappend processed_hands [list $category $mapped_cards $cards $bid]
    }
    set sorted_processed_hands [lsort $processed_hands]
    set total 0
    set N [llength $sorted_processed_hands]
    for {set i 0} {$i < $N} {incr i} {
        set hand_data [lindex $sorted_processed_hands $i]
        set bid [lindex $hand_data 3]
        set rank [expr {$i + 1}]
        set total [expr {$total + $bid * $rank}]
    }
    puts $total
}

main
