proc readAll {path} {
    set f [open $path r]
    set data [read $f]
    close $f
    return [string trim $data]
}

proc SetOf {b} {
    array set m {}
    foreach c $b {
        set m($c) 1
    }
    return [array names m]
}

proc firstNUnique {s n} {
    set len [string length $s]
    for {set i $n} {$i < $len} {incr i} {
        set b [string range $s [expr {$i - $n}] $i]
        if {[string length $b] == [llength [SetOf [split $b ""]]]} {
            return $i
        }
    }
    return -1
}

set s [readAll "input.txt"]
puts [firstNUnique $s 4]