proc calculate_fuel {mass} {
    set total_fuel 0
    set fuel [expr {int($mass / 3) - 2}]
    
    while {$fuel > 0} {
        set total_fuel [expr {$total_fuel + $fuel}]
        set fuel [expr {int($fuel / 3) - 2}]
    }
    
    return $total_fuel
}

proc main {} {
    set total_fuel_part1 0
    set total_fuel_part2 0

    set infile [open "input.txt" r]
    while {[gets $infile line] >= 0} {
        set mass [string trim $line]
        set fuel1 [expr {int($mass / 3) - 2}]
        set fuel2 [calculate_fuel $mass]

        set total_fuel_part1 [expr {$total_fuel_part1 + $fuel1}]
        set total_fuel_part2 [expr {$total_fuel_part2 + $fuel2}]
    }
    close $infile

    puts "Total fuel requirement (Part 1): $total_fuel_part1"
    puts "Total fuel requirement (Part 2): $total_fuel_part2"
}

main