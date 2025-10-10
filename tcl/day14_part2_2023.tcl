
set f [open input.txt]
set lines [split [read $f] \n]
close $f

set height [llength $lines]
set width [string length [lindex $lines 0]]

# Build grid
for {set y 0} {$y < $height} {incr y} {
    set row [lindex $lines $y]
    for {set x 0} {$x < $width} {incr x} {
        set c [string index $row $x]
        if {$c ne "."} {
            set grid($x,$y) $c
        }
    }
}

proc shiftSingle {x y dx dy} {
    global grid width height
    if {[info exists grid($x,$y)] && $grid($x,$y) eq "O"} {
        set cx $x; set cy $y
        while {1} {
            set nx [expr {$cx + $dx}]
            set ny [expr {$cy + $dy}]
            if {$nx < 0 || $nx >= $width || $ny < 0 || $ny >= $height} break
            if {[info exists grid($nx,$ny)]} break
            set grid($nx,$ny) "O"
            unset grid($cx,$cy)
            set cx $nx; set cy $ny
        }
    }
}

proc shiftDir {dx dy} {
    global height width
    if {$dy < 0 || $dx < 0} {
        for {set x 0} {$x < $width} {incr x} {
            for {set y 0} {$y < $height} {incr y} {
                shiftSingle $x $y $dx $dy
            }
        }
    } else {
        for {set x [expr {$width - 1}]} {$x >= 0} {incr x -1} {
            for {set y [expr {$height - 1}]} {$y >= 0} {incr y -1} {
                shiftSingle $x $y $dx $dy
            }
        }
    }
}

proc cycle {} {
    shiftDir 0 -1
    shiftDir -1 0
    shiftDir 0 1
    shiftDir 1 0
}

proc gridKey {} {
    global grid width height
    set key 0
    foreach c [array names grid] {
        if {$grid($c) eq "O"} {
            lassign [split $c ,] x y
            set key [expr {$key + $x + $y * $width}]
        }
    }
    return $key
}

proc load {} {
    global grid width height
    set load 0
    foreach c [array names grid] {
        if {$grid($c) eq "O"} {
            lassign [split $c ,] x y
            set load [expr {$load + $height - $y}]
        }
    }
    return $load
}

set numCycles 1000000000
array set cache {}
for {set i 0} {$i < $numCycles} {incr i} {
    set k [gridKey]
    if {[info exists cache($k)]} {
        set remaining [expr {($numCycles - $cache($k)) % ($i - $cache($k))}]
        for {set j 0} {$j < $remaining} {incr j} {cycle}
        puts [load]
        exit
    }
    set cache($k) $i
    cycle
}
puts [load]
