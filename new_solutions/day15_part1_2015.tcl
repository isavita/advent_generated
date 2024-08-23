proc readIngredients {filename} {
    set ingredients {}
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        regexp {(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)} $line -> name capacity durability flavor texture calories
        lappend ingredients [list $name $capacity $durability $flavor $texture $calories]
    }
    close $file
    return $ingredients
}

proc max {a b} {
    if {$a > $b} {return $a} else {return $b}
}

proc calculateScore {ingredients amounts} {
    set capacity 0
    set durability 0
    set flavor 0
    set texture 0

    for {set i 0} {$i < [llength $ingredients]} {incr i} {
        set ingredient [lindex $ingredients $i]
        set amount [lindex $amounts $i]
        set capacity [expr {$capacity + [lindex $ingredient 1] * $amount}]
        set durability [expr {$durability + [lindex $ingredient 2] * $amount}]
        set flavor [expr {$flavor + [lindex $ingredient 3] * $amount}]
        set texture [expr {$texture + [lindex $ingredient 4] * $amount}]
    }

    set capacity [max $capacity 0]
    set durability [max $durability 0]
    set flavor [max $flavor 0]
    set texture [max $texture 0]

    return [expr {$capacity * $durability * $flavor * $texture}]
}

proc findBestScore {ingredients} {
    set bestScore 0
    set n [llength $ingredients]

    proc dfs {ingredients amounts index remaining bestScore} {
        if {$index == [llength $ingredients] - 1} {
            lset amounts $index $remaining
            set score [calculateScore $ingredients $amounts]
            return [max $bestScore $score]
        }

        for {set i 0} {$i <= $remaining} {incr i} {
            lset amounts $index $i
            set bestScore [dfs $ingredients $amounts [expr {$index + 1}] [expr {$remaining - $i}] $bestScore]
        }
        return $bestScore
    }

    set amounts [list]
    for {set i 0} {$i < $n} {incr i} {lappend amounts 0}
    return [dfs $ingredients $amounts 0 100 $bestScore]
}

set ingredients [readIngredients "input.txt"]
set bestScore [findBestScore $ingredients]
puts $bestScore