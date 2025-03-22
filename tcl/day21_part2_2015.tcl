
proc parse_stat {line} {
    regexp {:\s+(\d+)} $line -> val
    return $val
}

proc player_wins {player_hp player_damage player_armor boss_hp boss_damage boss_armor} {
    set player_damage [expr {max(1, $player_damage - $boss_armor)}]
    set boss_damage [expr {max(1, $boss_damage - $player_armor)}]

    set player_turns [expr {($boss_hp + $player_damage - 1) / $player_damage}]
    set boss_turns [expr {($player_hp + $boss_damage - 1) / $boss_damage}]

    return [expr {$player_turns <= $boss_turns}]
}

proc max {a b} {
    if {$a > $b} {return $a} else {return $b}
}

set f [open "input.txt" r]
set lines [split [read $f] \n]
close $f

set boss_hp [parse_stat [lindex $lines 0]]
set boss_damage [parse_stat [lindex $lines 1]]
set boss_armor [parse_stat [lindex $lines 2]]

set weapons {
    {cost 8 damage 4}
    {cost 10 damage 5}
    {cost 25 damage 6}
    {cost 40 damage 7}
    {cost 74 damage 8}
}

set armors {
    {cost 0 armor 0}
    {cost 13 armor 1}
    {cost 31 armor 2}
    {cost 53 armor 3}
    {cost 75 armor 4}
    {cost 102 armor 5}
}

set rings {
    {cost 0 damage 0 armor 0}
    {cost 25 damage 1 armor 0}
    {cost 50 damage 2 armor 0}
    {cost 100 damage 3 armor 0}
    {cost 20 damage 0 armor 1}
    {cost 40 damage 0 armor 2}
    {cost 80 damage 0 armor 3}
}

set max_cost 0

foreach w $weapons {
    foreach a $armors {
        for {set ri 0} {$ri < [llength $rings]} {incr ri} {
            for {set rj [expr {$ri + 1}]} {$rj < [llength $rings]} {incr rj} {
                set w_cost [dict get $w cost]
                set w_damage [dict get $w damage]
                set a_cost [dict get $a cost]
                set a_armor [dict get $a armor]

                set ring_i [lindex $rings $ri]
                set ring_j [lindex $rings $rj]

                set ri_cost [dict get $ring_i cost]
                set ri_damage [dict get $ring_i damage]
                set ri_armor [dict get $ring_i armor]
                
                set rj_cost [dict get $ring_j cost]
                set rj_damage [dict get $ring_j damage]
                set rj_armor [dict get $ring_j armor]

                set player_hp 100
                set player_damage [expr {$w_damage + $ri_damage + $rj_damage}]
                set player_armor [expr {$a_armor + $ri_armor + $rj_armor}]

                set cost [expr {$w_cost + $a_cost + $ri_cost + $rj_cost}]

                if {![player_wins $player_hp $player_damage $player_armor $boss_hp $boss_damage $boss_armor] && $cost > $max_cost} {
                    set max_cost $cost
                }
            }
        }
    }
}

puts $max_cost
