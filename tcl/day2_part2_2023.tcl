proc parseCubes {cubes} {
    set red 0
    set green 0
    set blue 0
    foreach cube $cubes {
        if {[string match "*red" $cube]} {
            set red [expr {$red + [string trimright $cube " red"]}]
        } elseif {[string match "*green" $cube]} {
            set green [expr {$green + [string trimright $cube " green"]}]
        } elseif {[string match "*blue" $cube]} {
            set blue [expr {$blue + [string trimright $cube " blue"]}]
        }
    }
    return [list $red $green $blue]
}

set file [open "input.txt" r]
set totalPower 0

while {[gets $file line] >= 0} {
    set game [split [lindex [split $line ":"] 1] ";"]
    set minRed 0
    set minGreen 0
    set minBlue 0

    foreach round $game {
        set cubes [split $round ","]
        set counts [parseCubes $cubes]
        set minRed [expr {max($minRed, [lindex $counts 0])}]
        set minGreen [expr {max($minGreen, [lindex $counts 1])}]
        set minBlue [expr {max($minBlue, [lindex $counts 2])}]
    }

    set power [expr {$minRed * $minGreen * $minBlue}]
    set totalPower [expr {$totalPower + $power}]
}

close $file
puts $totalPower

proc max {a b} {
    return [expr {$a > $b ? $a : $b}]
}