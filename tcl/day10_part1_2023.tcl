
proc solve {input} {
    set grid [buildGrid $input]
    set start [findStart $grid]
    set path [pathFinding $start $grid]
    set numPipesVisited [llength $path]
    set maxLength [expr {$numPipesVisited / 2}]
    return $maxLength
}

proc buildGrid {input} {
    set width [string length [lindex $input 0]]
    set height [llength $input]
    set data {}
    for {set y 0} {$y < $height} {incr y} {
        set line [lindex $input $y]
        for {set x 0} {$x < $width} {incr x} {
            set char [string index $line $x]
            if {$char ne "."} {
                dict set data [list $x $y] $char
            }
        }
    }
    return [list width $width height $height data $data]
}

proc findStart {grid} {
    dict for {coord value} [dict get $grid data] {
        if {$value eq "S"} {
            return $coord
        }
    }
    return {}
}

proc getPipeFromNeighbors {coord grid} {
    set pipe {}
    set x [lindex $coord 0]
    set y [lindex $coord 1]
    set possibleNeighbors {
        {0 -1} {1 0} {0 1} {-1 0}
    }
    set directions {
        top right bottom left
    }
    foreach dir $possibleNeighbors direction $directions {
        set nx [expr {$x + [lindex $dir 0]}]
        set ny [expr {$y + [lindex $dir 1]}]
        if {[dict exists [dict get $grid data] [list $nx $ny]]} {
            set neighborPipe [getPipeFromTile [dict get [dict get $grid data] [list $nx $ny]]]
            set oppositeDir [oppositeDirection $direction]
            if {[dict exists $neighborPipe $oppositeDir]} {
                dict set pipe $direction {}
            }
        }
    }
    return $pipe
}

proc oppositeDirection {dir} {
    switch $dir {
        top {return bottom}
        right {return left}
        bottom {return top}
        left {return right}
    }
}

proc getPipeFromTile {tile} {
    set TileToPipe {
        "|" {top {} bottom {}}
        "-" {left {} right {}}
        "J" {top {} left {}}
        "L" {top {} right {}}
        "7" {bottom {} left {}}
        "F" {bottom {} right {}}
    }
    if {[dict exists $TileToPipe $tile]} {
        return [dict get $TileToPipe $tile]
    }
    return {}
}

proc getTileFromPipe {pipe} {
    set TileToPipe {
        "|" {top {} bottom {}}
        "-" {left {} right {}}
        "J" {top {} left {}}
        "L" {top {} right {}}
        "7" {bottom {} left {}}
        "F" {bottom {} right {}}
    }
    dict for {tile associatedPipe} $TileToPipe {
        if {[isEqualPipe $pipe $associatedPipe]} {
            return $tile
        }
    }
    return "."
}

proc isEqualPipe {pipe1 pipe2} {
    if {[dict size $pipe1] != [dict size $pipe2]} {
        return 0
    }
    dict for {dir _} $pipe1 {
        if {![dict exists $pipe2 $dir]} {
            return 0
        }
    }
    return 1
}

proc pathFinding {start grid} {
    set path [list $start]
    set startPipe [getPipeFromNeighbors $start $grid]
    
    foreach dir [dict keys $startPipe] {
        set previousDir $dir
        set current [addCoord $start [directionToCoord $dir]]
        break
    }
    
    while {$current ne $start} {
        lappend path $current
        set currentPipe [getPipeFromTile [dict get [dict get $grid data] $current]]
        foreach dir [dict keys $currentPipe] {
            if {$dir ne [oppositeDirection $previousDir]} {
                set previousDir $dir
                set current [addCoord $current [directionToCoord $dir]]
                break
            }
        }
    }
    return $path
}

proc addCoord {coord1 coord2} {
    return [list [expr {[lindex $coord1 0] + [lindex $coord2 0]}] [expr {[lindex $coord1 1] + [lindex $coord2 1]}]]
}

proc directionToCoord {dir} {
    switch $dir {
        top {return [list 0 -1]}
        right {return [list 1 0]}
        bottom {return [list 0 1]}
        left {return [list -1 0]}
    }
}

proc readFile {fileName} {
    set fileId [open $fileName r]
    set content [read $fileId]
    close $fileId
    return [split [string trim $content] "\n"]
}

set input [readFile "input.txt"]
puts [solve $input]
