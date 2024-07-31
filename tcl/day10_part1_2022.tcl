set x {1}
set sum 0
set i 0

proc readAll {path} {
    set file [open $path r]
    set content [read $file]
    close $file
    return $content
}

foreach line [split [readAll "input.txt"] "\n"] {
    if {$line eq "noop"} {
        lappend x [lindex $x end]
    } else {
        regexp {addx (-?\d+)} $line match n
        lappend x [lindex $x end]
        lappend x [expr {[lindex $x end] + $n}]
    }
}

foreach val $x {
    if {($i - 19) % 40 == 0} {
        set sum [expr {$sum + ($i + 1) * $val}]
    }
    incr i
}
puts $sum