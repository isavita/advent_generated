
#!/usr/bin/env tclsh

proc abs {x} {
    return [expr {abs($x)}]
}

proc parseInput {input} {
    set current {0 0}
    set vertices [list $current]
    
    foreach line $input {
        lassign $line dir length
        
        switch $dir {
            U { set move {0 -1} }
            L { set move {-1 0} }
            D { set move {0 1} }
            R { set move {1 0} }
        }
        
        set current [list \
            [expr {[lindex $current 0] + [lindex $move 0] * $length}] \
            [expr {[lindex $current 1] + [lindex $move 1] * $length}] \
        ]
        
        lappend vertices $current
    }
    
    return $vertices
}

proc shoelace {vertices} {
    set n [llength $vertices]
    set area 0
    
    for {set i 0} {$i < $n} {incr i} {
        set next [expr {($i + 1) % $n}]
        set v1 [lindex $vertices $i]
        set v2 [lindex $vertices $next]
        
        set area [expr {$area + [lindex $v1 0] * [lindex $v2 1] - [lindex $v1 1] * [lindex $v2 0]}]
    }
    
    return [expr {abs($area) / 2}]
}

proc perimeter {vertices} {
    set n [llength $vertices]
    set perim 0
    
    for {set i 0} {$i < $n} {incr i} {
        set next [expr {($i + 1) % $n}]
        set v1 [lindex $vertices $i]
        set v2 [lindex $vertices $next]
        
        set perim [expr {$perim + abs([lindex $v1 0] - [lindex $v2 0]) + abs([lindex $v1 1] - [lindex $v2 1])}]
    }
    
    return $perim
}

proc calculatePolygonArea {vertices} {
    return [expr {[shoelace $vertices] + [perimeter $vertices]/2 + 1}]
}

proc solve {input} {
    set vertices [parseInput $input]
    return [calculatePolygonArea $vertices]
}

# Read input from file
set fp [open "input.txt" r]
set input [split [read $fp] "\n"]
close $fp

# Remove any empty lines
set input [lsearch -all -inline -not -exact $input ""]

puts [solve $input]
