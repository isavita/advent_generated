
#!/usr/bin/env tclsh

proc readInput {} {
    set fp [open "input.txt" r]
    set numbers [split [gets $fp] ","]
    set boards {}
    
    while {[gets $fp line] != -1} {
        if {$line eq ""} {
            set board {}
            set marked {}
            for {set i 0} {$i < 5} {incr i} {
                gets $fp boardLine
                set boardRow [regexp -all -inline {\S+} $boardLine]
                set markedRow [lrepeat 5 0]
                lappend board $boardRow
                lappend marked $markedRow
            }
            lappend boards [list $board $marked]
        }
    }
    close $fp
    return [list $numbers $boards]
}

proc markBoard {board number} {
    lassign $board boardNumbers boardMarked
    set newMarked $boardMarked
    
    for {set i 0} {$i < 5} {incr i} {
        for {set j 0} {$j < 5} {incr j} {
            if {[lindex $boardNumbers $i $j] == $number} {
                lset newMarked $i $j 1
            }
        }
    }
    
    return [list [lindex $board 0] $newMarked]
}

proc hasWon {board} {
    lassign $board boardNumbers boardMarked
    
    # Check rows
    for {set i 0} {$i < 5} {incr i} {
        set rowWin 1
        for {set j 0} {$j < 5} {incr j} {
            if {[lindex $boardMarked $i $j] == 0} {
                set rowWin 0
                break
            }
        }
        if {$rowWin} {return 1}
    }
    
    # Check columns
    for {set j 0} {$j < 5} {incr j} {
        set colWin 1
        for {set i 0} {$i < 5} {incr i} {
            if {[lindex $boardMarked $i $j] == 0} {
                set colWin 0
                break
            }
        }
        if {$colWin} {return 1}
    }
    
    return 0
}

proc unmarkedSum {board} {
    lassign $board boardNumbers boardMarked
    set sum 0
    
    for {set i 0} {$i < 5} {incr i} {
        for {set j 0} {$j < 5} {incr j} {
            if {[lindex $boardMarked $i $j] == 0} {
                incr sum [lindex $boardNumbers $i $j]
            }
        }
    }
    
    return $sum
}

proc solve {} {
    lassign [readInput] numbers boards
    
    foreach number $numbers {
        set winningBoard {}
        for {set i 0} {$i < [llength $boards]} {incr i} {
            set board [lindex $boards $i]
            set newBoard [markBoard $board $number]
            lset boards $i $newBoard
            
            if {[hasWon $newBoard]} {
                set winningBoard $newBoard
                break
            }
        }
        
        if {$winningBoard ne {}} {
            set unmarkedSum [unmarkedSum $winningBoard]
            puts [expr {$unmarkedSum * $number}]
            return
        }
    }
}

solve
