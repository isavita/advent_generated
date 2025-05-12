
proc main {} {
    set sum 0
    set patterns {
        {0 0} {1 1} {2 2} {3 3} {4 4} {5 5} {6 6} {7 7} {8 8} {9 9}
        {zero 0} {one 1} {two 2} {three 3} {four 4} {five 5}
        {six 6} {seven 7} {eight 8} {nine 9}
    }

    set fileId [open "input.txt" r]
    while {[gets $fileId line] != -1} {
        set first_val -1
        set last_val -1
        set first_found 0

        for {set i 0} {$i < [string length $line]} {incr i} {
            set found_digit -1

            foreach pair $patterns {
                lassign $pair pattern value
                if {$i + [string length $pattern] <= [string length $line] && [string range $line $i [expr {$i + [string length $pattern] - 1}]] eq $pattern} {
                    set found_digit $value
                    break
                }
            }

            if {$found_digit != -1} {
                if {!$first_found} {
                    set first_val $found_digit
                    set first_found 1
                }
                set last_val $found_digit
            }
        }
        incr sum [expr {10 * $first_val + $last_val}]
    }
    close $fileId

    puts $sum
}

main
