
#!/usr/bin/tclsh

proc read_input {filename} {
    set fp [open $filename r]
    set content [read $fp]
    close $fp
    return $content
}

proc parse_input {content} {
    set lines [split $content "\n"]
    set draws [split [lindex $lines 0] ","]
    set boards {}
    set board {}
    foreach line [lrange $lines 2 end] {
        if {$line eq ""} {
            if {[llength $board] > 0} {
                lappend boards $board
                set board {}
            }
            continue
        }
        set row {}
        foreach num [split $line] {
            if {$num ne ""} {
                lappend row [list $num 0] ;# [number, marked]
            }
        }
        lappend board $row
    }
    # Add the last board if there is one
    if {[llength $board] > 0} {
        lappend boards $board
    }
    return [list $draws $boards]
}

proc mark_board {board draw} {
    set new_board {}
    foreach row $board {
        set new_row {}
        foreach cell $row {
            set num [lindex $cell 0]
            set marked [lindex $cell 1]
            if {$num eq $draw} {
                lappend new_row [list $num 1]
            } else {
                lappend new_row $cell
            }
        }
        lappend new_board $new_row
    }
    return $new_board
}

proc check_win {board} {
    # Check rows
    foreach row $board {
        set win 1
        foreach cell $row {
            if {[lindex $cell 1] == 0} {
                set win 0
                break
            }
        }
        if {$win} {
            return 1
        }
    }

    # Check columns
    for {set i 0} {$i < 5} {incr i} {
        set win 1
        for {set j 0} {$j < 5} {incr j} {
            if {[lindex [lindex $board $j] $i 1] == 0} {
                set win 0
                break
            }
        }
        if {$win} {
            return 1
        }
    }
    return 0
}

proc calculate_score {board last_draw} {
    set unmarked_sum 0
    foreach row $board {
        foreach cell $row {
            set num [lindex $cell 0]
            set marked [lindex $cell 1]
            if {!$marked} {
                incr unmarked_sum $num
            }
        }
    }
    return [expr {$unmarked_sum * $last_draw}]
}

proc solve_bingo {draws boards} {
    set winning_boards {}
    set last_winning_score 0
    set last_winning_board {}
    set last_draw 0

    foreach draw $draws {
        set new_boards {}
        for {set i 0} {$i < [llength $boards]} {incr i} {
            set board [lindex $boards $i]
            if {[lsearch -exact $winning_boards $i] >= 0} {
                # This board already won
                lappend new_boards $board
                continue
            }
            set new_board [mark_board $board $draw]
            lappend new_boards $new_board

            if {[check_win $new_board]} {
                lappend winning_boards $i
                set last_winning_score [calculate_score $new_board $draw]
                set last_winning_board $new_board
                set last_draw $draw
            }
        }
        set boards $new_boards
    }

    return [list $last_winning_board $last_winning_score $last_draw]
}

# Main entry point
set content [read_input "input.txt"]
set data [parse_input $content]
set draws [lindex $data 0]
set boards [lindex $data 1]

set result [solve_bingo $draws $boards]
set last_winning_board [lindex $result 0]
set last_winning_score [lindex $result 1]
set last_draw [lindex $result 2]

puts "Final score of the board that wins last: $last_winning_score (last draw was $last_draw)"
