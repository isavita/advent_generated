
#!/usr/bin/env tclsh

proc readInput {} {
    set fp [open "input.txt" r]
    set data [read $fp]
    close $fp
    return [split [string trim $data] "\n"]
}

proc moveEast {gridVar} {
    upvar $gridVar grid
    set height [llength $grid]
    set width [string length [lindex $grid 0]]
    set moved 0
    set newGrid $grid

    for {set y 0} {$y < $height} {incr y} {
        for {set x 0} {$x < $width} {incr x} {
            set nextX [expr {($x + 1) % $width}]
            if {[string index [lindex $grid $y] $x] eq ">" && 
                [string index [lindex $grid $y] $nextX] eq "."} {
                set row [lindex $newGrid $y]
                set row [string replace $row $x $x "."]
                set row [string replace $row $nextX $nextX ">"]
                lset newGrid $y $row
                set moved 1
                incr x
            }
        }
    }
    set grid $newGrid
    return $moved
}

proc moveSouth {gridVar} {
    upvar $gridVar grid
    set height [llength $grid]
    set width [string length [lindex $grid 0]]
    set moved 0
    set newGrid $grid

    for {set x 0} {$x < $width} {incr x} {
        for {set y 0} {$y < $height} {incr y} {
            set nextY [expr {($y + 1) % $height}]
            set currentChar [string index [lindex $grid $y] $x]
            set nextChar [string index [lindex $grid $nextY] $x]
            
            if {$currentChar eq "v" && $nextChar eq "."} {
                set currentRow [lindex $newGrid $y]
                set nextRow [lindex $newGrid $nextY]
                
                set currentRow [string replace $currentRow $x $x "."]
                set nextRow [string replace $nextRow $x $x "v"]
                
                lset newGrid $y $currentRow
                lset newGrid $nextY $nextRow
                
                set moved 1
                incr y
            }
        }
    }
    set grid $newGrid
    return $moved
}

proc findSafeStep {grid} {
    set step 0
    while {1} {
        set eastMoved [moveEast grid]
        set southMoved [moveSouth grid]
        incr step

        if {!$eastMoved && !$southMoved} {
            break
        }
    }
    return $step
}

set input [readInput]
set result [findSafeStep $input]
puts $result
