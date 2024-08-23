set file [open "input.txt" r]
set directions [read $file]
close $file

array set blackTiles {}

proc parseLine {line} {
    set x 0
    set y 0
    set i 0
    set len [string length $line]

    while {$i < $len} {
        set dir [string index $line $i]
        if {$dir eq "n" || $dir eq "s"} {
            set dir [string range $line $i [expr {$i + 1}]]
            incr i
        }
        switch -- $dir {
            "e" { incr x }
            "w" { incr x -1 }
            "ne" { incr y; incr x }
            "nw" { incr y }
            "se" { incr y -1 }
            "sw" { incr y -1; incr x -1 }
        }
        incr i
    }
    return "$x,$y"
}

foreach line [split $directions "\n"] {
    set coord [parseLine $line]
    if {[info exists blackTiles($coord)]} {
        unset blackTiles($coord)
    } else {
        set blackTiles($coord) 1
    }
}

puts [array size blackTiles]