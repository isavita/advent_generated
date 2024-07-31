set file [open "input.txt" r]
set totalScore 0

while {[gets $file line] >= 0} {
    set opponent [string index $line 0]
    set yourMove [string index $line 2]

    set score 0
    if {$yourMove == "X"} {set score 1}
    if {$yourMove == "Y"} {set score 2}
    if {$yourMove == "Z"} {set score 3}

    if {($opponent == "A" && $yourMove == "Y") || ($opponent == "B" && $yourMove == "Z") || ($opponent == "C" && $yourMove == "X")} {
        set score [expr {$score + 6}]
    } elseif {($opponent == "A" && $yourMove == "X") || ($opponent == "B" && $yourMove == "Y") || ($opponent == "C" && $yourMove == "Z")} {
        set score [expr {$score + 3}]
    }

    set totalScore [expr {$totalScore + $score}]
}

close $file
puts $totalScore