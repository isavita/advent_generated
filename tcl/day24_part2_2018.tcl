
proc calc_dmg {atk def} {
    set ep [expr {[dict get $atk units] * [dict get $atk dmg]}]
    set type [dict get $atk type]
    if {[lsearch -exact [dict get $def imm] $type] != -1} {return 0}
    if {[lsearch -exact [dict get $def weak] $type] != -1} {return [expr {$ep * 2}]}
    return $ep
}

proc compare_selection {gd a b} {
    set ga [dict get $gd $a]; set gb [dict get $gd $b]
    set epa [expr {[dict get $ga units] * [dict get $ga dmg]}]
    set epb [expr {[dict get $gb units] * [dict get $gb dmg]}]
    if {$epa != $epb} {return [expr {$epb - $epa}]}
    return [expr {[dict get $gb init] - [dict get $ga init]}]
}

proc compare_init {gd a b} {
    return [expr {[dict get [dict get $gd $b] init] - [dict get [dict get $gd $a] init]}]
}

proc simulate {boost groups_dict} {
    dict for {id g} $groups_dict {
        if {[dict get $g army] eq "immune"} {
            dict set groups_dict $id dmg [expr {[dict get $g dmg] + $boost}]
        }
    }
    while {1} {
        set imm 0; set inf 0
        dict for {id g} $groups_dict {
            if {[dict get $g units] > 0} {
                if {[dict get $g army] eq "immune"} {incr imm} else {incr inf}
            }
        }
        if {$imm == 0 || $inf == 0} break
        set attackers [list]
        dict for {id g} $groups_dict { if {[dict get $g units] > 0} {lappend attackers $id} }
        set attackers [lsort -command [list compare_selection $groups_dict] $attackers]
        set targets [dict create]; set targeted [dict create]
        foreach aid $attackers {
            set atk [dict get $groups_dict $aid]; set best_dmg 0; set best_tid -1
            dict for {tid def} $groups_dict {
                if {[dict get $def units] <= 0 || [dict get $def army] eq [dict get $atk army] || [dict exists $targeted $tid]} continue
                set d [calc_dmg $atk $def]
                if {$d > $best_dmg} {
                    set best_dmg $d; set best_tid $tid
                } elseif {$d == $best_dmg && $d > 0} {
                    set bdef [dict get $groups_dict $best_tid]
                    set ep_n [expr {[dict get $def units] * [dict get $def dmg]}]
                    set ep_o [expr {[dict get $bdef units] * [dict get $bdef dmg]}]
                    if {$ep_n > $ep_o || ($ep_n == $ep_o && [dict get $def init] > [dict get $bdef init])} {set best_tid $tid}
                }
            }
            if {$best_tid != -1} {dict set targets $aid $best_tid; dict set targeted $best_tid 1}
        }
        set attack_ids [list]
        dict for {id g} $groups_dict { if {[dict get $g units] > 0} {lappend attack_ids $id} }
        set attack_ids [lsort -command [list compare_init $groups_dict] $attack_ids]
        set total_killed 0
        foreach aid $attack_ids {
            if {![dict exists $groups_dict $aid]} continue
            set atk [dict get $groups_dict $aid]
            if {[dict get $atk units] <= 0 || ![dict exists $targets $aid]} continue
            set tid [dict get $targets $aid]; set def [dict get $groups_dict $tid]
            set killed [expr {[calc_dmg $atk $def] / [dict get $def hp]}]
            if {$killed > [dict get $def units]} {set killed [dict get $def units]}
            incr total_killed $killed
            dict set groups_dict $tid units [expr {[dict get $def units] - $killed}]
        }
        if {$total_killed == 0} {return {none 0}}
    }
    set win "none"; set u 0
    dict for {id g} $groups_dict {
        if {[dict get $g units] > 0} {set win [dict get $g army]; incr u [dict get $g units]}
    }
    return [list $win $u]
}

set f [open "input.txt" r]; set data [read $f]; close $f
set id_gen 0; set groups [dict create]; set army ""
foreach line [split $data "\n"] {
    set line [string trim $line]
    if {$line eq ""} continue
    if {[regexp {Immune System:} $line]} {set army "immune"; continue}
    if {[regexp {Infection:} $line]} {set army "infection"; continue}
    if {[regexp {(\d+) units each with (\d+) hit points (.*)with an attack that does (\d+) (\w+) damage at initiative (\d+)} $line -> u h m d t i]} {
        set imm {}; set weak {}
        if {[regexp {\((.*)\)} $m -> mods]} {
            foreach p [split $mods ";"] {
                set p [string trim $p]
                if {[regexp {immune to (.*)} $p -> l]} {foreach x [split $l ,] {lappend imm [string trim $x]}}
                if {[regexp {weak to (.*)} $p -> l]} {foreach x [split $l ,] {lappend weak [string trim $x]}}
            }
        }
        set id [incr id_gen]
        dict set groups $id [dict create units $u hp $h dmg $d type $t init $i imm $imm weak $weak army $army id $id]
    }
}

set low 0; set high 100000; set ans 0
while {$low <= $high} {
    set mid [expr {($low + $high) / 2}]
    lassign [simulate $mid $groups] winner units
    if {$winner eq "immune"} {set ans $units; set high [expr {$mid - 1}]} else {set low [expr {$mid + 1}]}
}
puts $ans
