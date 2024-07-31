set score 0
set depth 0
set inGarbage 0
set cancelNext 0

set file [open "input.txt" r]
set content [read $file]
close $file

foreach ch [split $content ""] {
    if {$cancelNext} {
        set cancelNext 0
        continue
    }

    if {$inGarbage} {
        if {$ch eq "!"} {
            set cancelNext 1
        } elseif {$ch eq ">"} {
            set inGarbage 0
        }
    } else {
        switch -- $ch {
            "{" {
                incr depth
            }
            "}" {
                set score [expr {$score + $depth}]
                incr depth -1
            }
            "<" {
                set inGarbage 1
            }
        }
    }
}

puts $score