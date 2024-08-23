proc readFile {filename} {
    set file [open $filename r]
    set replacements {}
    set molecule ""

    while {[gets $file line] >= 0} {
        if {$line eq ""} {
            continue
        }
        if {[regexp {(.+) => (.+)} $line match from to]} {
            lappend replacements [list $from $to]
        } else {
            set molecule $line
        }
    }
    close $file
    return [list $replacements $molecule]
}

proc countSteps {molecule} {
    set elements [regexp -all -inline {[A-Z][a-z]?} $molecule]
    set count [llength $elements]
    set rn [regexp -all -inline {Rn} $molecule]
    set ar [regexp -all -inline {Ar} $molecule]
    set y [regexp -all -inline {Y} $molecule]
    return [expr {$count - [llength $rn] - [llength $ar] - 2 * [llength $y] - 1}]
}

set data [readFile "input.txt"]
set replacements [lindex $data 0]
set molecule [lindex $data 1]

puts [countSteps $molecule]