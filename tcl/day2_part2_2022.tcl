set file [open "input.txt" r]
set totalScore 0

while {[gets $file line] >= 0} {
    set opponent [string index $line 0]
    set outcome [string index $line 2]
    set score 0

    switch -- $outcome {
        X {
            switch -- $opponent {
                A {set score 3}
                B {set score 1}
                C {set score 2}
            }
        }
        Y {
            switch -- $opponent {
                A {set score 4}
                B {set score 5}
                C {set score 6}
            }
        }
        Z {
            switch -- $opponent {
                A {set score 8}
                B {set score 9}
                C {set score 7}
            }
        }
    }
    set totalScore [expr {$totalScore + $score}]
}

close $file
puts $totalScore