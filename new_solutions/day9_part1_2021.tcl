set file [open "input.txt" r]
set heightmap {}
while {[gets $file line] >= 0} {
    lappend heightmap [split $line ""]
}
close $file

proc isLowPoint {heightmap x y} {
    set current [lindex [lindex $heightmap $x] $y]
    set directions {{-1 0} {1 0} {0 -1} {0 1}}
    foreach dir $directions {
        set nx [expr {$x + [lindex $dir 0]}]
        set ny [expr {$y + [lindex $dir 1]}]
        if {$nx >= 0 && $nx < [llength $heightmap] && $ny >= 0 && $ny < [llength [lindex $heightmap 0]]} {
            if {[lindex [lindex $heightmap $nx] $ny] <= $current} {
                return 0
            }
        }
    }
    return 1
}

set riskLevelSum 0
for {set i 0} {$i < [llength $heightmap]} {incr i} {
    for {set j 0} {$j < [llength [lindex $heightmap 0]]} {incr j} {
        if {[isLowPoint $heightmap $i $j]} {
            set riskLevelSum [expr {$riskLevelSum + [lindex [lindex $heightmap $i] $j] + 1}]
        }
    }
}

puts $riskLevelSum