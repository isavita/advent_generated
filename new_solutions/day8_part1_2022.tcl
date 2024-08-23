set file [open "input.txt" r]
set grid {}
while {[gets $file line] >= 0} {
    lappend grid [split $line ""]
}
close $file

set rows [llength $grid]
set cols [llength [lindex $grid 0]]
set visibleTrees 0

for {set i 0} {$i < $rows} {incr i} {
    for {set j 0} {$j < $cols} {incr j} {
        if {$i == 0 || $i == $rows-1 || $j == 0 || $j == $cols-1} {
            incr visibleTrees
        } else {
            set height [lindex [lindex $grid $i] $j]
            set visible 1
            for {set k 0} {$k < $j} {incr k} {
                if {[lindex [lindex $grid $i] $k] >= $height} {
                    set visible 0
                    break
                }
            }
            if {$visible} {
                incr visibleTrees
                continue
            }
            set visible 1
            for {set k [expr {$j + 1}]} {$k < $cols} {incr k} {
                if {[lindex [lindex $grid $i] $k] >= $height} {
                    set visible 0
                    break
                }
            }
            if {$visible} {
                incr visibleTrees
                continue
            }
            set visible 1
            for {set k 0} {$k < $i} {incr k} {
                if {[lindex [lindex $grid $k] $j] >= $height} {
                    set visible 0
                    break
                }
            }
            if {$visible} {
                incr visibleTrees
                continue
            }
            set visible 1
            for {set k [expr {$i + 1}]} {$k < $rows} {incr k} {
                if {[lindex [lindex $grid $k] $j] >= $height} {
                    set visible 0
                    break
                }
            }
            if {$visible} {
                incr visibleTrees
            }
        }
    }
}
puts $visibleTrees