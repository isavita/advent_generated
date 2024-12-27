
proc add {c1 c2} {
    list [expr {[lindex $c1 0] + [lindex $c2 0]}] [expr {[lindex $c1 1] + [lindex $c2 1]}]
}

proc rotate90 {coord} {
    list [lindex $coord 1] [expr {-[lindex $coord 0]}]
}

proc rotateNeg90 {coord} {
    list [expr {-[lindex $coord 1]}] [lindex $coord 0]
}

proc isInBounds {coord width height} {
    set x [lindex $coord 0]
    set y [lindex $coord 1]
    expr {$x >= 0 && $x < $width && $y >= 0 && $y < $height}
}

proc buildGrid {input} {
    set width [string length [lindex $input 0]]
    set height [llength $input]
    set data [dict create]
    
    for {set y 0} {$y < $height} {incr y} {
        set line [lindex $input $y]
        for {set x 0} {$x < $width} {incr x} {
            set char [string index $line $x]
            if {$char ne "."} {
                dict set data [list $x $y] $char
            }
        }
    }
    return [list $width $height $data]
}

proc nextBeam {grid beam} {
    lassign $grid width height data
    lassign $beam origin dir
    
    if {![dict exists $data $origin]} {
        return [list [list [add $origin $dir] $dir]]
    }
    
    set char [dict get $data $origin]
    
    switch $char {
        "/" {
            if {[lindex $dir 0] == 0} {
                set newDir [rotateNeg90 $dir]
            } else {
                set newDir [rotate90 $dir]
            }
            return [list [list [add $origin $newDir] $newDir]]
        }
        "\\" {
            if {[lindex $dir 0] == 0} {
                set newDir [rotate90 $dir]
            } else {
                set newDir [rotateNeg90 $dir]
            }
            return [list [list [add $origin $newDir] $newDir]]
        }
        "|" {
            if {[lindex $dir 0] != 0} {
                set newDir1 [rotate90 $dir]
                set newDir2 [rotateNeg90 $dir]
                return [list [list [add $origin $newDir1] $newDir1] [list [add $origin $newDir2] $newDir2]]
            }
        }
        "-" {
            if {[lindex $dir 0] == 0} {
                set newDir1 [rotate90 $dir]
                set newDir2 [rotateNeg90 $dir]
                return [list [list [add $origin $newDir1] $newDir1] [list [add $origin $newDir2] $newDir2]]
            }
        }
    }
    return [list [list [add $origin $dir] $dir]]
}

proc calculatePropagation {grid start} {
    set alreadySeen [dict create]
    set toExplore [list $start]
    
    while {[llength $toExplore] > 0} {
        set beam [lindex $toExplore 0]
        set toExplore [lrange $toExplore 1 end]
        lassign $beam origin dir
        lassign $grid width height data
        
        if {[isInBounds $origin $width $height] && ![dict exists $alreadySeen $beam]} {
            dict set alreadySeen $beam ""
            set nextBeams [nextBeam $grid $beam]
            foreach nextBeam $nextBeams {
                lappend toExplore $nextBeam
            }
        }
    }
    return $alreadySeen
}

proc calculateEnergization {alreadySeen} {
    set alreadyEnergized [dict create]
    dict for {beam _} $alreadySeen {
        lassign $beam origin dir
        if {![dict exists $alreadyEnergized $origin]} {
            dict set alreadyEnergized $origin ""
        }
    }
    return $alreadyEnergized
}

proc getBorder {grid} {
    lassign $grid width height data
    set border {}
    
    for {set x 0} {$x < $width} {incr x} {
        lappend border [list [list $x 0] [list 0 1]]
        lappend border [list [list $x [expr {$height - 1}]] [list 0 -1]]
    }
    
    for {set y 0} {$y < $height} {incr y} {
        lappend border [list [list 0 $y] [list 1 0]]
        lappend border [list [list [expr {$width - 1}] $y] [list -1 0]]
    }
    return $border
}

proc solve {input} {
    set grid [buildGrid $input]
    set starts [getBorder $grid]
    set res 0
    
    foreach start $starts {
        set alreadySeen [calculatePropagation $grid $start]
        set alreadyEnergized [calculateEnergization $alreadySeen]
        set energy [dict size $alreadyEnergized]
        if {$energy > $res} {
            set res $energy
        }
    }
    return $res
}

set file [open "input.txt" r]
set content [read $file]
close $file
set input [split [string trim $content] "\n"]
puts [solve $input]
