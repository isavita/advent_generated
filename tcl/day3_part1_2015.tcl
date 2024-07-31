set file [open "input.txt" r]
set directions [read $file]
close $file

set visitedHouses [dict create]
set x 0
set y 0

dict set visitedHouses "$x,$y" 1

foreach dir [split $directions ""] {
    switch $dir {
        "^" { incr y }
        "v" { incr y -1 }
        ">" { incr x }
        "<" { incr x -1 }
    }
    dict set visitedHouses "$x,$y" 1
}

puts [dict size $visitedHouses]