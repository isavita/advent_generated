
proc attempt {x y dx dy} {
    global grid
    set char $grid($x,$y)
    if {$char eq "."} {return 1}
    if {$char eq "#"} {return 0}
    set nx [expr {$x + $dx}]; set ny [expr {$y + $dy}]
    set backup [array get grid]
    if {$char eq "@" || $char eq "O"} {
        if {[attempt $nx $ny $dx $dy]} {
            set grid($nx,$ny) $char
            set grid($x,$y) "."
            return 1
        }
    } elseif {$char eq "\["} {
        if {$dy != 0} {
            if {[attempt $nx $ny $dx $dy] && [attempt [expr {$nx+1}] $ny $dx $dy]} {
                set grid($nx,$ny) "\["; set grid([expr {$nx+1}],$ny) "\]"
                set grid($x,$y) "."; set grid([expr {$x+1}],$y) "."
                return 1
            }
        } else {
            if {$dx == 1} {
                if {[attempt [expr {$x+2}] $y $dx $dy]} {
                    set grid([expr {$x+1}],$y) "\["; set grid([expr {$x+2}],$y) "\]"
                    set grid($x,$y) "."
                    return 1
                }
            } else {
                if {[attempt [expr {$x-1}] $y $dx $dy]} {
                    set grid([expr {$x-1}],$y) "\["; set grid($x,$y) "\]"
                    set grid([expr {$x+1}],$y) "."
                    return 1
                }
            }
        }
    } elseif {$char eq "\]"} {
        if {[attempt [expr {$x-1}] $y $dx $dy]} {return 1}
    }
    array set grid $backup
    return 0
}

proc solve {mapStr moveStr} {
    global grid robotX robotY
    array unset grid
    set y 0
    foreach line [split [string trim $mapStr "\n"] "\n"] {
        set x 0
        foreach char [split $line ""] {
            set grid($x,$y) $char
            if {$char eq "@"} {set robotX $x; set robotY $y}
            incr x
        }
        incr y
    }
    foreach m [split $moveStr ""] {
        set dx 0; set dy 0
        if {$m eq "^"} {set dy -1} elseif {$m eq "v"} {set dy 1} \
        elseif {$m eq "<"} {set dx -1} elseif {$m eq ">"} {set dx 1} \
        else continue
        if {[attempt $robotX $robotY $dx $dy]} {
            set robotX [expr {$robotX + $dx}]
            set robotY [expr {$robotY + $dy}]
        }
    }
    set score 0
    foreach {key val} [array get grid] {
        if {$val eq "O" || $val eq "\["} {
            lassign [split $key ","] x y
            set score [expr {$score + 100 * $y + $x}]
        }
    }
    return $score
}

set f [open "input.txt" r]
set content [read $f]
close $f
set content [string map {"\r\n" "\n"} $content]
set firstBlank [string first "\n\n" $content]
set mapStr [string range $content 0 $firstBlank]
set moveStr [string map {"\n" "" " " ""} [string range $content [expr {$firstBlank+2}] end]]

puts [solve $mapStr $moveStr]

set map2 ""
foreach line [split [string trim $mapStr "\n"] "\n"] {
    set newLine ""
    foreach char [split $line ""] {
        switch -- $char {
            "#" { append newLine "##" }
            "O" { append newLine "\[\]" }
            "." { append newLine ".." }
            "@" { append newLine "@." }
        }
    }
    append map2 $newLine "\n"
}
puts [solve $map2 $moveStr]
