proc md5 {s} {
    set result [exec echo -n $s | openssl md5]
    return [string trim [string range $result 0 end-1]]
}

proc isOpen {c} {
    return [expr {[string first $c "bcdef"] != -1}]
}

proc bfs {passcode} {
    set queue [list [list 0 0 ""]]
    while {[llength $queue] > 0} {
        set current [lindex $queue 0]
        set queue [lrange $queue 1 end]
        set x [lindex $current 0]
        set y [lindex $current 1]
        set path [lindex $current 2]

        if {$x == 3 && $y == 3} {
            return $path
        }

        set hash [md5 "${passcode}${path}"]
        set up [isOpen [string index $hash 0]]
        set down [isOpen [string index $hash 1]]
        set left [isOpen [string index $hash 2]]
        set right [isOpen [string index $hash 3]]

        if {$up && $y > 0} {
            lappend queue [list $x [expr {$y - 1}] "${path}U"]
        }
        if {$down && $y < 3} {
            lappend queue [list $x [expr {$y + 1}] "${path}D"]
        }
        if {$left && $x > 0} {
            lappend queue [list [expr {$x - 1}] $y "${path}L"]
        }
        if {$right && $x < 3} {
            lappend queue [list [expr {$x + 1}] $y "${path}R"]
        }
    }
    return ""
}

set file [open "input.txt" r]
set passcode [string trim [read $file]]
close $file

set shortestPath [bfs $passcode]
puts $shortestPath