proc readInput {filename} {
    set file [open $filename r]
    set depth 0
    set target {}
    while {[gets $file line] >= 0} {
        if {[string match "depth: *" $line]} {
            set depth [string trim [string range $line 7 end]]
        } elseif {[string match "target: *" $line]} {
            set target [split [string trim [string range $line 8 end]] ","]
        }
    }
    close $file
    return [list $depth $target]
}

proc calculateRiskLevel {depth target} {
    array set geologicIndex {}
    array set erosionLevel {}
    set riskLevel 0

    for {set y 0} {$y <= [lindex $target 1]} {incr y} {
        for {set x 0} {$x <= [lindex $target 0]} {incr x} {
            if {$x == 0 && $y == 0} {
                set geologicIndex($x,$y) 0
            } elseif {$x == [lindex $target 0] && $y == [lindex $target 1]} {
                set geologicIndex($x,$y) 0
            } elseif {$y == 0} {
                set geologicIndex($x,$y) [expr {$x * 16807}]
            } elseif {$x == 0} {
                set geologicIndex($x,$y) [expr {$y * 48271}]
            } else {
                set geologicIndex($x,$y) [expr {$erosionLevel([expr {$x - 1}],$y) * $erosionLevel($x,[expr {$y - 1}])}]
            }
            set erosionLevel($x,$y) [expr {($geologicIndex($x,$y) + $depth) % 20183}]
            set type [expr {$erosionLevel($x,$y) % 3}]
            set riskLevel [expr {$riskLevel + $type}]
        }
    }
    return $riskLevel
}

set input [readInput "input.txt"]
set depth [lindex $input 0]
set target [lindex $input 1]
set riskLevel [calculateRiskLevel $depth $target]
puts $riskLevel