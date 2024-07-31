proc calculate_presents {house_number} {
    set total_presents 0
    for {set elf 1} {$elf * $elf <= $house_number} {incr elf} {
        if {$house_number % $elf == 0} {
            set total_presents [expr {$total_presents + $elf * 10}]
            set divisor [expr {$house_number / $elf}]
            if {$divisor != $elf} {
                set total_presents [expr {$total_presents + $divisor * 10}]
            }
        }
    }
    return $total_presents
}

proc find_lowest_house {target_presents} {
    set house 1
    while {1} {
        if {[calculate_presents $house] >= $target_presents} {
            return $house
        }
        incr house
    }
}

set infile [open "input.txt" r]
set target_presents [read $infile]
close $infile

set lowest_house [find_lowest_house $target_presents]
puts $lowest_house