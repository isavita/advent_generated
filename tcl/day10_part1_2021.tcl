
proc checkLine {line scoreVar} {
    upvar $scoreVar score
    set stack {}
    set score 0
    foreach c [split $line ""] {
        switch $c {
            "(" - "\[" - "\{" - "<" { lappend stack $c }
            ")" - "\]" - "\}" - ">" {
                if {[llength $stack] == 0} { return 0 }
                set last [lindex $stack end]
                set expected [dict get { ")" "(" "\]" "\[" "\}" "\{" ">" "<" } $c]
                if {$last ne $expected} {
                    set score [dict get { ")" 3 "\]" 57 "\}" 1197 ">" 25137 } $c]
                    return 1
                }
                set stack [lrange $stack 0 end-1]
            }
        }
    }
    return 0
}

set totalScore 0
set f [open input.txt r]
while {[gets $f line] >= 0} {
    set score 0
    if {[checkLine $line score]} { incr totalScore $score }
}
close $f
puts $totalScore
