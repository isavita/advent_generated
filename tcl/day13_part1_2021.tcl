proc readInput {filename} {
    set dots {}
    set folds {}
    set readingDots 1
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        if {[string equal $line ""]} {
            set readingDots 0
            continue
        }
        if {$readingDots} {
            lappend dots [split $line ,]
        } else {
            lappend folds [lindex [split $line " "] 2]
        }
    }
    close $file
    return [list $dots $folds]
}

proc foldDots {dots fold} {
    set axis [lindex $fold 0]
    set line [lindex $fold 1]
    set newDots {}
    foreach dot $dots {
        set x [lindex $dot 0]
        set y [lindex $dot 1]
        if {$axis == "x"} {
            if {$x > $line} {
                set x [expr {$line - ($x - $line)}]
            }
        } elseif {$axis == "y"} {
            if {$y > $line} {
                set y [expr {$line - ($y - $line)}]
            }
        }
        lappend newDots [list $x $y]
    }
    return [lsort -unique $newDots]
}

proc main {} {
    set data [readInput "input.txt"]
    set dots [lindex $data 0]
    set folds [lindex $data 1]

    if {[llength $folds] > 0} {
        set firstFold [split [lindex $folds 0] =]
        set axis [lindex $firstFold 0]
        set line [lindex $firstFold 1]
        set foldedDots [foldDots $dots [list $axis $line]]
        puts [llength $foldedDots]
    }
}

main