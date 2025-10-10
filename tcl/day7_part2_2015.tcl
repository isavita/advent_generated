
proc readFile {filename} {
    set f [open $filename r]
    set data [read $f]
    close $f
    return $data
}

proc memoDFS {graph entry memo} {
    upvar $memo m
    if {[info exists m($entry)]} {
        return $m($entry)
    }
    if {[string is integer -strict $entry]} {
        return [expr {$entry & 0xFFFF}]
    }
    set rule [dict get $graph $entry]
    set parts [split $rule]
    set result 0
    if {[llength $parts] == 1} {
        set result [memoDFS $graph [lindex $parts 0] m]
    } elseif {[lindex $parts 0] eq "NOT"} {
        set result [expr {(0xFFFF ^ [memoDFS $graph [lindex $parts 1] m]) & 0xFFFF}]
    } elseif {[lindex $parts 1] eq "AND"} {
        set result [expr {([memoDFS $graph [lindex $parts 0] m] & [memoDFS $graph [lindex $parts 2] m]) & 0xFFFF}]
    } elseif {[lindex $parts 1] eq "OR"} {
        set result [expr {([memoDFS $graph [lindex $parts 0] m] | [memoDFS $graph [lindex $parts 2] m]) & 0xFFFF}]
    } elseif {[lindex $parts 1] eq "LSHIFT"} {
        set result [expr {([memoDFS $graph [lindex $parts 0] m] << [memoDFS $graph [lindex $parts 2] m]) & 0xFFFF}]
    } elseif {[lindex $parts 1] eq "RSHIFT"} {
        set result [expr {([memoDFS $graph [lindex $parts 0] m] >> [memoDFS $graph [lindex $parts 2] m]) & 0xFFFF}]
    }
    set m($entry) $result
    return $result
}

proc solve {input} {
    set graph [dict create]
    foreach line [split $input \n] {
        if {$line eq ""} continue
        set arrow [string first " -> " $line]
        set rule [string range $line 0 $arrow-1]
        set wire [string range $line $arrow+4 end]
        dict set graph $wire $rule
    }
    array unset memo1
    set aSignal [memoDFS $graph a memo1]
    dict set graph b $aSignal
    array unset memo2
    return [memoDFS $graph a memo2]
}

set input [readFile input.txt]
puts [solve $input]
