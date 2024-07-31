set file [open "input.txt" r]
set totalCount 0
set groupAnswers {}
set groupSize 0

while {[gets $file line] >= 0} {
    if {$line eq ""} {
        foreach count [dict values $groupAnswers] {
            if {$count == $groupSize} {
                incr totalCount
            }
        }
        set groupAnswers {}
        set groupSize 0
    } else {
        incr groupSize
        foreach question [split $line ""] {
            dict incr groupAnswers $question
        }
    }
}

foreach count [dict values $groupAnswers] {
    if {$count == $groupSize} {
        incr totalCount
    }
}

close $file
puts $totalCount