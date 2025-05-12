
proc main {} {
    set f [open input.txt r]
    set input [read $f]
    close $f

    set lines [split [string trim $input] "\n"]
    set processed_lines [list]
    foreach line $lines {
        if {[string trim $line] ne ""} {
            lappend processed_lines $line
        }
    }
    set lines $processed_lines

    set num_cards [llength $lines]

    array set card_counts {}
    for {set i 0} {$i < $num_cards} {incr i} {
        set card_counts($i) 1
    }

    for {set i 0} {$i < $num_cards} {incr i} {
        set line [lindex $lines $i]

        if {[regexp {:\s*(.*)\s*\|\s*(.*)$} $line dummy winnings_str givens_str]} {
            array set winning_nums_map {}
            foreach num [regexp -all -inline {[0-9]+} $winnings_str] {
                set winning_nums_map($num) 1
            }

            set match_count 0
            foreach num [regexp -all -inline {[0-9]+} $givens_str] {
                if {[info exists winning_nums_map($num)]} {
                    incr match_count
                }
            }
            unset winning_nums_map

            for {set j 1} {$j <= $match_count} {incr j} {
                set target_idx [expr {$i + $j}]
                if {$target_idx < $num_cards} {
                    incr card_counts($target_idx) $card_counts($i)
                }
            }
        }
    }

    set total_cards 0
    if {[array exists card_counts]} {
      foreach idx [array names card_counts] {
          incr total_cards $card_counts($idx)
      }
    }

    puts $total_cards
}

main
