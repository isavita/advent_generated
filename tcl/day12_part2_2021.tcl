proc readInput {filename} {
    set graph [dict create]
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        set parts [split $line "-"]
        set a [lindex $parts 0]
        set b [lindex $parts 1]
        if {[dict exists $graph $a]} {
            dict set graph $a [concat [dict get $graph $a] $b]
        } else {
            dict set graph $a [list $b]
        }
        if {[dict exists $graph $b]} {
            dict set graph $b [concat [dict get $graph $b] $a]
        } else {
            dict set graph $b [list $a]
        }
    }
    close $file
    return $graph
}

proc isSmallCave {cave} {
    return [string is lower $cave]
}

proc countPaths {graph visited current visitedTwice} {
    if {$current eq "end"} {
        return 1
    }
    if {[isSmallCave $current] && [dict exists $visited $current]} {
        if {$visitedTwice} {
            return 0
        }
        set visitedTwice 1
    }
    dict incr visited $current
    set paths 0
    foreach neighbor [dict get $graph $current] {
        if {$neighbor ne "start"} {
            set paths [expr {$paths + [countPaths $graph $visited $neighbor $visitedTwice]}]
        }
    }
    dict incr visited $current -1
    return $paths
}

set graph [readInput "input.txt"]
set visited [dict create]
puts [countPaths $graph $visited "start" 0]