
#!/usr/bin/env tclsh

proc parseInput {filePath} {
    set file [open $filePath r]
    set lines [split [read $file] "\n"]
    close $file

    set initialState [string index [lindex $lines 0] end-1]
    set steps [regexp -inline {\d+} [lindex $lines 1]]

    array set states {}
    for {set i 3} {$i < [llength $lines]} {incr i 10} {
        set state [string index [lindex $lines $i] end-1]
        set value0 [string index [lindex $lines [expr $i+2]] end-1]
        set move0 [expr {[string first "left" [lindex $lines [expr $i+3]]] != -1 ? -1 : 1}]
        set nextState0 [string index [lindex $lines [expr $i+4]] end-1]
        
        set value1 [string index [lindex $lines [expr $i+6]] end-1]
        set move1 [expr {[string first "left" [lindex $lines [expr $i+7]]] != -1 ? -1 : 1}]
        set nextState1 [string index [lindex $lines [expr $i+8]] end-1]
        
        set states($state,0) [list $value0 $move0 $nextState0]
        set states($state,1) [list $value1 $move1 $nextState1]
    }

    return [list $initialState $steps [array get states]]
}

proc runTuringMachine {filePath} {
    lassign [parseInput $filePath] state steps statesData
    array set states $statesData

    array set tape {}
    set cursor 0
    set checksum 0

    for {set i 0} {$i < $steps} {incr i} {
        if {![info exists tape($cursor)]} {
            set tape($cursor) 0
        }
        
        set currentValue $tape($cursor)
        lassign $states($state,$currentValue) newValue move nextState

        set tape($cursor) $newValue
        incr cursor $move
        set state $nextState
    }

    foreach value [array names tape] {
        incr checksum $tape($value)
    }

    return $checksum
}

set result [runTuringMachine "input.txt"]
puts $result
