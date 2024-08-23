proc readInput {filename} {
    set dots {}
    set folds {}
    set readingDots 1

    set file [open $filename r]
    while {[gets $file line] >= 0} {
        if {$line eq ""} {
            set readingDots 0
            continue
        }
        if {$readingDots} {
            lappend dots [split $line ","]
        } else {
            lappend folds [string range $line 11 end]
        }
    }
    close $file
    return [list $dots $folds]
}

proc foldDots {dots fold} {
    set axis [string index $fold 0]
    set value [string range $fold 2 end]
    set value [expr {$value + 0}]
    set foldedDots {}

    foreach dot $dots {
        set x [lindex $dot 0]
        set y [lindex $dot 1]
        if {$axis eq "x" && $x > $value} {
            set x [expr {$value - ($x - $value)}]
        } elseif {$axis eq "y" && $y > $value} {
            set y [expr {$value - ($y - $value)}]
        }
        lappend foldedDots [list $x $y]
    }

    return [lsort -unique $foldedDots]
}

proc printDots {dots} {
    set maxX 0
    set maxY 0
    foreach dot $dots {
        set x [lindex $dot 0]
        set y [lindex $dot 1]
        if {$x > $maxX} {set maxX $x}
        if {$y > $maxY} {set maxY $y}
    }

    for {set y 0} {$y <= $maxY} {incr y} {
        for {set x 0} {$x <= $maxX} {incr x} {
            if {[lsearch -exact $dots [list $x $y]] >= 0} {
                puts -nonewline "#"
            } else {
                puts -nonewline "."
            }
        }
        puts ""
    }
}

set input [readInput "input.txt"]
set dots [lindex $input 0]
set folds [lindex $input 1]

foreach fold $folds {
    set dots [foldDots $dots $fold]
}

printDots $dots