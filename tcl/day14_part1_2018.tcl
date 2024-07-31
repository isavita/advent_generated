proc main {} {
    set input [open "input.txt" r]
    set numRecipes [gets $input]
    close $input

    set scoreboard [list 3 7]
    set elf1 0
    set elf2 1

    while {[llength $scoreboard] < $numRecipes + 10} {
        set sum [expr {[lindex $scoreboard $elf1] + [lindex $scoreboard $elf2]}]
        foreach digit [split $sum ""] {
            lappend scoreboard $digit
        }
        set elf1 [expr {($elf1 + 1 + [lindex $scoreboard $elf1]) % [llength $scoreboard]}]
        set elf2 [expr {($elf2 + 1 + [lindex $scoreboard $elf2]) % [llength $scoreboard]}]
    }

    set result [join [lrange $scoreboard $numRecipes [expr {$numRecipes + 9}]] ""]
    puts $result
}

main