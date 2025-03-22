
proc battle {player_hp player_damage player_armor boss_hp boss_damage boss_armor} {
    set player_turns [expr {ceil(double($boss_hp) / max(1, $player_damage - $boss_armor))}]
    set boss_turns [expr {ceil(double($player_hp) / max(1, $boss_damage - $player_armor))}]
    return [expr {$player_turns <= $boss_turns}]
}

proc main {} {
    set weapons {
        {Dagger 8 4 0}
        {Shortsword 10 5 0}
        {Warhammer 25 6 0}
        {Longsword 40 7 0}
        {Greataxe 74 8 0}
    }

    set armor {
        {None 0 0 0}
        {Leather 13 0 1}
        {Chainmail 31 0 2}
        {Splintmail 53 0 3}
        {Bandedmail 75 0 4}
        {Platemail 102 0 5}
    }

    set rings {
        {None 0 0 0}
        {"Damage +1" 25 1 0}
        {"Damage +2" 50 2 0}
        {"Damage +3" 100 3 0}
        {"Defense +1" 20 0 1}
        {"Defense +2" 40 0 2}
        {"Defense +3" 80 0 3}
    }

    set boss_stats {103 9 2} ;# Hit points, damage, armor
    set player_stats {100 0 0} ;# Hit points, damage, armor

    set min_gold "inf"

    foreach weapon $weapons {
        lassign $weapon w_name w_cost w_damage w_armor
        foreach armor_choice $armor {
            lassign $armor_choice a_name a_cost a_damage a_armor
            for {set i 0} {$i < [llength $rings]} {incr i} {
                for {set j [expr {$i + 1}]} {$j < [llength $rings]} {incr j} {
                    set ring1 [lindex $rings $i]
                    set ring2 [lindex $rings $j]
                    lassign $ring1 r1_name r1_cost r1_damage r1_armor
                    lassign $ring2 r2_name r2_cost r2_damage r2_armor

                    set cost [expr {$w_cost + $a_cost + $r1_cost + $r2_cost}]
                    set player_damage [expr {$w_damage + $a_damage + $r1_damage + $r2_damage}]
                    set player_armor [expr {$w_armor + $a_armor + $r1_armor + $r2_armor}]

                    if {[battle [lindex $player_stats 0] $player_damage $player_armor [lindex $boss_stats 0] [lindex $boss_stats 1] [lindex $boss_stats 2]]} {
                        if {$cost < $min_gold} {
                            set min_gold $cost
                        }
                    }
                }
            }
        }
    }
    puts $min_gold
}

main
