
proc main {} {
    set f [open input.txt r]
    set initialState ""
    array set rules {}
    while {[gets $f line] >= 0} {
        if {[string match "*initial state*" $line]} {
            regexp {initial state: ([.#]*)} $line -> initialState
        } elseif {[string match "*=>*" $line]} {
            regexp {([.#]{5}) => ([.#])} $line -> pat res
            set rules($pat) $res
        }
    }
    close $f

    set pots [dict create]
    set idx 0
    foreach c [split $initialState ""] {
        if {$c eq "#"} {dict set pots $idx 1}
        incr idx
    }

    for {set gen 0} {$gen < 20} {incr gen} {
        set new [dict create]
        set min [expr {[lindex [lsort -integer [dict keys $pots]] 0] - 2}]
        set max [expr {[lindex [lsort -integer [dict keys $pots]] end] + 2}]
        for {set i $min} {$i <= $max} {incr i} {
            set pat ""
            for {set d -2} {$d <= 2} {incr d} {
                append pat [expr {[dict exists $pots [expr {$i + $d}]] ? "#" : "."}]
            }
            if {[info exists rules($pat)] && $rules($pat) eq "#"} {
                dict set new $i 1
            }
        }
        set pots $new
    }

    set sum 0
    foreach k [dict keys $pots] {incr sum $k}
    puts $sum
}
main
