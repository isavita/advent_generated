proc count_trees {filename right down} {
    set file [open $filename r]
    set map [split [read $file] "\n"]
    close $file

    set width [string length [lindex $map 0]]
    set height [llength $map]
    set x 0
    set y 0
    set trees 0

    while {$y < $height} {
        if {[string index [lindex $map $y] $x] eq "#"} {
            incr trees
        }
        set x [expr {($x + $right) % $width}]
        set y [expr {$y + $down}]
    }

    return $trees
}

set result [count_trees "input.txt" 3 1]
puts $result