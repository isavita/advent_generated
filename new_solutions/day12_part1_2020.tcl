proc main {} {
    set file [open "input.txt" r]
    set instructions [split [read $file] "\n"]
    close $file

    set directions {E S W N}
    set directionIndex 0
    set east 0
    set north 0

    foreach instruction $instructions {
        set action [string index $instruction 0]
        set value [string range $instruction 1 end]
        set value [expr {$value + 0}]

        switch -- $action {
            N {set north [expr {$north + $value}]}
            S {set north [expr {$north - $value}]}
            E {set east [expr {$east + $value}]}
            W {set east [expr {$east - $value}]}
            L {set directionIndex [expr {($directionIndex - $value / 90) % 4}]}
            R {set directionIndex [expr {($directionIndex + $value / 90) % 4}]}
            F {
                switch -- [lindex $directions $directionIndex] {
                    E {set east [expr {$east + $value}]}
                    S {set north [expr {$north - $value}]}
                    W {set east [expr {$east - $value}]}
                    N {set north [expr {$north + $value}]}
                }
            }
        }
        if {$directionIndex < 0} {set directionIndex [expr {$directionIndex + 4}]}
    }

    set manhattanDistance [expr {abs($east) + abs($north)}]
    puts $manhattanDistance
}

proc abs {x} {
    return [expr {$x < 0 ? -$x : $x}]
}

main