
set f [open input.txt r]
gets $f line
set bossHP [lindex [split $line :] 1]
gets $f line
set bossDamage [lindex [split $line :] 1]
close $f

set minMana 2147483647

proc simulate {playerHP playerMana bossHP bossDamage shieldTimer poisonTimer rechargeTimer manaSpent playerTurn} \
{
    global minMana
    if {$manaSpent >= $minMana} {return}
    if {$bossHP <= 0} {set minMana $manaSpent; return}
    if {$playerHP <= 0} {return}

    if {$playerTurn} {
        incr playerHP -1
        if {$playerHP <= 0} {return}
    }

    set armor 0
    if {$shieldTimer > 0} {
        set armor 7
        incr shieldTimer -1
    }
    if {$poisonTimer > 0} {
        incr bossHP -3
        incr poisonTimer -1
    }
    if {$rechargeTimer > 0} {
        incr playerMana 101
        incr rechargeTimer -1
    }

    if {!$playerTurn} {
        set damage [expr {$bossDamage - $armor}]
        if {$damage < 1} {set damage 1}
        incr playerHP -$damage
        simulate $playerHP $playerMana $bossHP $bossDamage $shieldTimer $poisonTimer $rechargeTimer $manaSpent 1
        return
    }

    if {$playerMana >= 53} {
        simulate $playerHP [expr {$playerMana-53}] [expr {$bossHP-4}] $bossDamage $shieldTimer $poisonTimer $rechargeTimer [expr {$manaSpent+53}] 0
    }
    if {$playerMana >= 73} {
        simulate [expr {$playerHP+2}] [expr {$playerMana-73}] [expr {$bossHP-2}] $bossDamage $shieldTimer $poisonTimer $rechargeTimer [expr {$manaSpent+73}] 0
    }
    if {$playerMana >= 113 && $shieldTimer == 0} {
        simulate $playerHP [expr {$playerMana-113}] $bossHP $bossDamage 6 $poisonTimer $rechargeTimer [expr {$manaSpent+113}] 0
    }
    if {$playerMana >= 173 && $poisonTimer == 0} {
        simulate $playerHP [expr {$playerMana-173}] $bossHP $bossDamage $shieldTimer 6 $rechargeTimer [expr {$manaSpent+173}] 0
    }
    if {$playerMana >= 229 && $rechargeTimer == 0} {
        simulate $playerHP [expr {$playerMana-229}] $bossHP $bossDamage $shieldTimer $poisonTimer 5 [expr {$manaSpent+229}] 0
    }
}

simulate 50 500 $bossHP $bossDamage 0 0 0 0 1
puts $minMana
