set file [open "input.txt" r]
set input [read $file]
close $file

proc abs {x} {
    return [expr {$x < 0 ? -$x : $x}]
}

proc max {a b} {
    return [expr {$a > $b ? $a : $b}]
}

proc distance {x y z} {
    return [expr {([abs $x] + [abs $y] + [abs $z]) / 2}]
}

set directions [split $input ","]
set x 0
set y 0
set z 0
set maxDistance 0

foreach dir $directions {
    switch -- $dir {
        "n" { incr y; incr z -1 }
        "ne" { incr x; incr z -1 }
        "se" { incr x; incr y -1 }
        "s" { incr y -1; incr z }
        "sw" { incr x -1; incr z }
        "nw" { incr x -1; incr y }
    }
    set curDistance [distance $x $y $z]
    set maxDistance [max $maxDistance $curDistance]
}

puts [distance $x $y $z]