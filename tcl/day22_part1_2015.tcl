
proc min {a b} { expr {$a < $b ? $a : $b} }
set INF 1000000000
set memo [dict create]
set bossDamage 0

proc solve {pHP pMana bHP sTimer pTimer rTimer turn} {
    global bossDamage memo INF

    set key [list $pHP $pMana $bHP $sTimer $pTimer $rTimer $turn]
    if {[dict exists $memo $key]} {
        return [dict get $memo $key]
    }

    set c_pHP $pHP
    set c_pMana $pMana
    set c_bHP $bHP
    set c_sTimer $sTimer
    set c_pTimer $pTimer
    set c_rTimer $rTimer

    if {$c_sTimer > 0} { incr c_sTimer -1 }
    if {$c_pTimer > 0} { incr c_bHP -3; incr c_pTimer -1 }
    if {$c_rTimer > 0} { incr c_pMana +101; incr c_rTimer -1 }

    if {$c_bHP <= 0} {
        dict set memo $key 0
        return 0
    }
    if {$c_pHP <= 0} {
        dict set memo $key $INF
        return $INF
    }

    set minAdditionalMana $INF

    if {$turn} {
        set results [list]
        # Magic Missile (53)
        if {$c_pMana >= 53} {
            set res [solve $c_pHP [expr {$c_pMana - 53}] [expr {$c_bHP - 4}] $c_sTimer $c_pTimer $c_rTimer 0]
            if {$res < $INF} { lappend results [expr {53 + $res}] }
        }
        # Drain (73)
        if {$c_pMana >= 73} {
            set res [solve [expr {$c_pHP + 2}] [expr {$c_pMana - 73}] [expr {$c_bHP - 2}] $c_sTimer $c_pTimer $c_rTimer 0]
            if {$res < $INF} { lappend results [expr {73 + $res}] }
        }
        # Shield (113)
        if {$c_pMana >= 113 && $c_sTimer == 0} {
            set res [solve $c_pHP [expr {$c_pMana - 113}] $c_bHP 6 $c_pTimer $c_rTimer 0]
            if {$res < $INF} { lappend results [expr {113 + $res}] }
        }
        # Poison (173)
        if {$c_pMana >= 173 && $c_pTimer == 0} {
            set res [solve $c_pHP [expr {$c_pMana - 173}] $c_bHP $c_sTimer 6 $c_rTimer 0]
            if {$res < $INF} { lappend results [expr {173 + $res}] }
        }
        # Recharge (229)
        if {$c_pMana >= 229 && $c_rTimer == 0} {
            set res [solve $c_pHP [expr {$c_pMana - 229}] $c_bHP $c_sTimer $c_pTimer 5 0]
            if {$res < $INF} { lappend results [expr {229 + $res}] }
        }

        if {[llength $results] == 0} {
            set minAdditionalMana $INF
        } else {
            set minAdditionalMana [lindex [lsort -integer $results] 0]
        }

    } else {
        set damage $bossDamage
        if {$c_sTimer > 0} { incr damage -7 }
        if {$damage < 1} { set damage 1 }
        set new_pHP [expr {$c_pHP - $damage}]
        set res [solve $new_pHP $c_pMana $c_bHP $c_sTimer $c_pTimer $c_rTimer 1]
        set minAdditionalMana $res
    }

    dict set memo $key $minAdditionalMana
    return $minAdditionalMana
}

if {[info exists argv0] && ([file tail $argv0] eq [file tail [info script]])} {
    set fid [open "input.txt" r]
    gets $fid line1
    gets $fid line2
    close $fid

    regexp {:\s*(\d+)} $line1 _ bossHP_str
    regexp {:\s*(\d+)} $line2 _ bossDamage_str

    set initialBossHP [expr {$bossHP_str}]
    set ::bossDamage [expr {$bossDamage_str}]

    set initial_pHP 50
    set initial_pMana 500
    set initial_sTimer 0
    set initial_pTimer 0
    set initial_rTimer 0
    set initial_turn 1

    set result [solve $initial_pHP $initial_pMana $initialBossHP $initial_sTimer $initial_pTimer $initial_rTimer $initial_turn]

    if {$result == $INF} {
        # puts "No winning path found"
    } else {
        puts $result
    }
}
