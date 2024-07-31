set file [open "input.txt" r]
set score 0
set depth 0
set inGarbage 0
set cancelNext 0
set garbageCount 0

while {[gets $file line] >= 0} {
    foreach ch [split $line ""] {
        if {$cancelNext} {
            set cancelNext 0
            continue
        }
        if {$inGarbage} {
            if {$ch == "!"} {
                set cancelNext 1
            } elseif {$ch == ">"} {
                set inGarbage 0
            } else {
                incr garbageCount
            }
        } else {
            switch -- $ch {
                "{" {incr depth}
                "}" {incr score $depth; incr depth -1}
                "<" {set inGarbage 1}
            }
        }
    }
}
close $file
puts $garbageCount