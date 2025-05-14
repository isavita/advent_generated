
proc alphabetize {str} {
    set chars [split $str ""]
    set sorted_chars [lsort $chars]
    return [join $sorted_chars ""]
}

proc checkOverlap {larger smaller} {
    foreach char [split $smaller ""] {
        if {[string first $char $larger] == -1} {
            return 0
        }
    }
    return 1
}

proc main {} {
    set f [open "input.txt" r]
    set input [read $f]
    close $f

    set total_sum 0

    foreach line [split [string trim $input] "\n"] {
        set parts [regexp -all -inline {[a-g]+} $line]
        set patterns_raw [lrange $parts 0 9]
        set outputs_raw [lrange $parts 10 end]

        set patterns [list]
        foreach p $patterns_raw {
            lappend patterns [alphabetize $p]
        }
        set outputs [list]
        foreach o $outputs_raw {
            lappend outputs [alphabetize $o]
        }

        array set mapping {}
        array set string_to_digit {}

        foreach p $patterns {
            switch [string length $p] {
                2 { set mapping(1) $p }
                3 { set mapping(7) $p }
                4 { set mapping(4) $p }
                7 { set mapping(8) $p }
            }
        }
        set string_to_digit($mapping(1)) 1
        set string_to_digit($mapping(4)) 4
        set string_to_digit($mapping(7)) 7
        set string_to_digit($mapping(8)) 8

        foreach p $patterns {
            if {[string length $p] == 6} {
                if {[checkOverlap $p $mapping(4)]} {
                    set mapping(9) $p
                } elseif {[checkOverlap $p $mapping(1)]} {
                    set mapping(0) $p
                } else {
                    set mapping(6) $p
                }
            }
        }
        set string_to_digit($mapping(0)) 0
        set string_to_digit($mapping(6)) 6
        set string_to_digit($mapping(9)) 9

        foreach p $patterns {
            if {[string length $p] == 5} {
                if {[checkOverlap $p $mapping(1)]} {
                    set mapping(3) $p
                } elseif {[checkOverlap $mapping(9) $p]} {
                    set mapping(5) $p
                } else {
                    set mapping(2) $p
                }
            }
        }
        set string_to_digit($mapping(2)) 2
        set string_to_digit($mapping(3)) 3
        set string_to_digit($mapping(5)) 5

        set output_value 0
        foreach output_pattern $outputs {
            set digit $string_to_digit($output_pattern)
            set output_value [expr {$output_value * 10 + $digit}]
        }

        set total_sum [expr {$total_sum + $output_value}]
    }

    puts $total_sum
}

main
