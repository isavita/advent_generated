proc readInput {filename} {
    set graph [dict create]
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        set parts [split $line "-"]
        set a [lindex $parts 0]
        set b [lindex $parts 1]
        if {![dict exists $graph $a]} {
            dict set graph $a {}
        }
        if {![dict exists $graph $b]} {
            dict set graph $b {}
        }
        dict set graph $a [concat [dict get $graph $a] $b]
        dict set graph $b [concat [dict get $graph $b] $a]
    }
    close $file
    return $graph
}

proc isSmallCave {cave} {
    return [string is lower $cave]
}

proc findPaths {graph current visited} {
    if {$current eq "end"} {
        return 1
    }
    if {[isSmallCave $current] && [dict exists $visited $current]} {
        return 0
    }
    dict set visited $current 1
    set paths 0
    foreach next [dict get $graph $current] {
        set paths [expr {$paths + [findPaths $graph $next $visited]}]
    }
    dict unset visited $current
    return $paths
}

set graph [readInput "input.txt"]
puts [findPaths $graph "start" [dict create]]